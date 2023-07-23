-module(refundcheck).

-include_lib("epgsql/include/epgsql.hrl").
-include_lib("kernel/include/logger.hrl").

% -feature(maybe_expr, enable). 

-export([
    login/1,
    isValidUserAPIKey/1,
    getSeller/1,
    getSellersMails/0,

    getPlan/1,
    payment_start/4,
    payment_complete/2,

    getColValues/2,
    registerPurchase/2,
    registerRefund/2,
    getCustomerHistory/2,
    getCustomerHistoryGlobal/1
]).


-define(KEY_SIZE, 40).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

login(#{
    name  := SellerName,
    email := SellerMail
    }) ->
    
    Conn = getConnection(),

    SellerData = case selectSellerIds(Conn, SellerMail) of
        [] -> % register seller
            SellerKey = refundcheck_config:newToken(?KEY_SIZE),
            SellerAvailCalls = refundcheck_config:getSellerInitialCalls(),
            insertSeller(Conn, SellerName, SellerMail, SellerKey, SellerAvailCalls),
            % LastPaymentDate = selectSellerLastPaymentDate(z) 
            #{
                seller_mail => SellerMail,
                seller_name => SellerName,
                seller_key  => SellerKey,
                seller_avail_calls => SellerAvailCalls
            };

        [SellerId] ->
            Seller = selectSeller(Conn, SellerId),
            #{
                seller_mail => maps:get(<<"mail">>, Seller),
                seller_name => maps:get(<<"name">>, Seller),
                seller_key  => maps:get(<<"key">>,  Seller),
                seller_avail_calls => maps:get(<<"available_calls">>, Seller)
            }
    end,
    closeConnection(Conn),
    SellerData.


isValidUserAPIKey(<<"">>) ->
    false;
isValidUserAPIKey(UserAPIKey) ->
    Conn = getConnection(),
    Seller = selectSellerByKey(Conn, UserAPIKey),
    IsValid = case Seller of
        [] -> false;
        _Other -> true
    end,
    closeConnection(Conn),
    IsValid.

getSeller(UserAPIKey) ->
    Conn = getConnection(),
    Seller = selectSellerByKey(Conn, UserAPIKey),
    closeConnection(Conn),
    Seller.

getSellersMails() ->
    Conn = getConnection(),
    Sellers = selectSellers(Conn),
    SellersMails = lists:filtermap(
        fun(#{<<"mail">> := SellersMail}) -> 
            {true, SellersMail}
        end, 
        Sellers
    ),
    closeConnection(Conn),
    #{
        result => <<"ok">>,
        data => SellersMails
    }.


getPlan(<<"plan-s01">>) ->
    #{
        amount => 9,
        calls  => 240
    };
getPlan(<<"plan-m01">>) ->
    #{
        amount => 19,
        calls  => 600
    };
getPlan(<<"plan-l01">>) ->
    #{
        amount => 69,
        calls  => 2400
    }.


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



payment_start(Seller, SKU, Amount, Data) ->
    #{<<"id">> := SellerId} = Seller,
    #{
        <<"status">> := Status,
        <<"id">> := TrxId
    } = Data,
    Conn = getConnection(),
    insertPayment(Conn, SellerId, Amount, SKU, 1, Data, TrxId, Status),
    closeConnection(Conn).


payment_complete(Seller, Data) ->
    #{
        <<"available_calls">> := SellerAvailCalls
    } = Seller,
    #{
        <<"status">> := Status,
        <<"id">> := TrxId
    } = Data,
    Conn = getConnection(),
    updatePayment(Conn, Data, TrxId, Status),
    #{<<"concept">> := SKU} = selectPayment(Conn, TrxId),
    #{calls := AdditionalCalls} = getPlan(SKU),
    UpdSeller = Seller#{
        <<"available_calls">> => SellerAvailCalls + AdditionalCalls
    },
    updateSeller(Conn, UpdSeller),
    closeConnection(Conn).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

registerPurchase(PurchaseData, Seller) ->

    %TODO validatePurchaseData(PurchaseData)

    #{
        <<"mail">>     := SellerMail
    } = Seller,
    #{
        customer_mail   := CustomerMail,
        trx_id          := TransactionId,
        product_type    := ProductTypeId,
        % date            := Date,
        amount_range    := AmountRangeId,
        amount_currency := AmountCurrency
    } = PurchaseData,

    io:format("PurchaseData:~p~n", [PurchaseData]),
    Conn = getConnection(),

    [SellerId] = selectSellerIds(Conn, SellerMail),

    CustomerIds = selectCustomerIds(Conn, CustomerMail),
    CustomerId = case CustomerIds of
        [] ->
            insertCustomer(Conn, CustomerMail),
            [NewCId] = selectCustomerIds(Conn, CustomerMail),
            NewCId;
        [CId] -> CId
    end,
    io:format("CustomerId:~p~n", [CustomerId]),

    % insertPurchase(Conn, SellerId, CustomerId, ProductTypeId, Date, AmountRangeId, AmountCurrency, TransactionId),
    Res = insertPurchase(Conn, SellerId, CustomerId, ProductTypeId, AmountRangeId, AmountCurrency, TransactionId),
    io:format("Insert Res:~p~n", [Res]),
    ok = closeConnection(Conn),
      
    R = case Res of
        {error,{error,error,_ErrorCode,unique_violation, _InternalDesc, _ErrorDetail}} ->
            #{
                result => <<"error">>,
                description => <<"transaction id already registered">>
            };
        {ok, 1} ->            
            #{
                result => <<"ok">>,
                description => <<"purchase has been registered">>
            };
        _ ->
            #{
                result => <<"error">>,
                description => <<"unknown">>
            }
    end,
    R.
    

registerRefund(RefundData, Seller) ->

    #{
        <<"mail">> := SellerMail
    } = Seller,
    #{
        purchase_trx_id := PurchaseTrxId,
        refund_type := RefundTypeId,
        extra_info := RefundDescription
    } = RefundData,
    
    Conn = getConnection(),

    [SellerId] = selectSellerIds(Conn, SellerMail),
    Res = insertRefund(Conn, SellerId, PurchaseTrxId, RefundTypeId, RefundDescription),
    io:format("Insert Res:~p~n", [Res]),
    ok = closeConnection(Conn),
      
    R = case Res of
        {ok, 1} ->            
            #{
                result => <<"ok">>,
                description => <<"refund has been registered">>
            };
        _ ->
            #{
                result => <<"error">>,
                description => <<"unknown">>
            }
    end,
    R.

    
%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


getCustomerHistory(SellerId, CustomerMail) ->
    Conn = getConnection(),

    CustomerIds = selectCustomerIds(Conn, CustomerMail),
    Res = getCustomerHistoryInternal(Conn, SellerId, CustomerIds),

    ok = closeConnection(Conn),
    Res.


getCustomerHistoryGlobal(CustomerMail) ->
    Conn = getConnection(),

    CustomerIds = selectCustomerIds(Conn, CustomerMail),
    Res = getCustomerHistoryInternal(Conn, [], CustomerIds),

    ok = closeConnection(Conn),
    Res.


getCustomerHistoryInternal(_Conn, _SellerId, []) ->
    #{
        result        => <<"ok">>,
        description   => <<"no data for this customer">>,
        purchases_num => 0,
        refunds_num   => 0,
        refunds_p     => 0
    };
getCustomerHistoryInternal(Conn, SellerId, [CustomerId]) ->
    % get customer's purchases history
    Purchases = selectPurchasesByCustomerAndSeller(Conn, CustomerId, SellerId),
    
    % get refunds history
    Refunds = selectRefundsByPurchases(Conn, Purchases),

    % consolidate refunds info
    % TODO expandir la info: datos recientes
    Res = consolidateRefundInfo(Purchases, Refunds),
    Res.


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

getConnection() ->
    Host     = refundcheck_config:getDBHost(),
    Username = refundcheck_config:getDBUser(), 
    Password = refundcheck_config:getDBPass(), %TODO  ### WARNING!!  use anon fun insted of plain text for the pass
    
    {ok, Conn} = epgsql:connect(Host, Username, Password, []),
    Conn.

closeConnection(Conn) ->
    epgsql:close(Conn).


getColValues(ResultSet, ColumnName) when is_atom(ColumnName) -> 
    getColValues(ResultSet, atom_to_binary(ColumnName));

getColValues(ResultSet, ColumnName) -> 
    ParsedResult = parse_result(ResultSet),
    mapColValues(ParsedResult, ColumnName).


mapColValues(ParsedResult, ColumnName) when is_list(ParsedResult) ->
    lists:map(
        fun(ResultMap) ->
            maps:get(ColumnName, ResultMap, undefined)
        end,
        ParsedResult
    );
mapColValues({ok, Counts, ResultMaps}, ColumnName) ->
    {ok, Counts, mapColValues(ResultMaps, ColumnName)}.


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

selectSellerIds(Conn, SellerMail) ->
    RS = epgsql:equery(Conn,
        "SELECT id FROM refundcheck.seller s WHERE s.mail = $1",
        [SellerMail]
    ),
    getColValues(RS, id).

selectSellers(Conn) ->
    RS = epgsql:equery(Conn,
        "SELECT * FROM refundcheck.seller s",
        []
    ),
    R = parse_result(RS),
    R.

selectSeller(Conn, SellerId) ->
    RS = epgsql:equery(Conn,
        "SELECT *
        FROM refundcheck.seller s
        WHERE s.id = $1",
        [SellerId]
    ),
    [R] = parse_result(RS),
    R.

selectSellerByKey(Conn, SellerKey) ->
    RS = epgsql:equery(Conn,
        "SELECT *
        FROM refundcheck.seller s
        WHERE s.key = $1",
        [SellerKey]
    ),
    R = case parse_result(RS) of
        [] -> [];
        [R1] -> R1
    end,
    R.


updateSeller(Conn, NewSeller) ->
    #{
        <<"id">> := SellerId,
        <<"name">> := SellerName,
        <<"mail">> := SellerMail,
        <<"key">> := SellerKey,
        <<"key_expires_in">> := SellerKeyExipresIn,
        <<"available_calls">> := SellerAvailCalls,
        <<"url">> := SellerURL
    } = NewSeller,
    epgsql:equery(Conn,
        "UPDATE refundcheck.seller
	        SET name=$1, mail=$2, key=$3, key_expires_in=$4, available_calls=$5, url=$6
	        WHERE id = $7",
        [SellerName, SellerMail, SellerKey, SellerKeyExipresIn, SellerAvailCalls, SellerURL, SellerId]
    ).




insertPayment(Conn, SellerId, Amount, Concept, Items, Data, TrxId, Status) ->
    DataStr = jsx:encode(Data),
    epgsql:equery(Conn,
        "INSERT INTO refundcheck.payment (
            seller_id, amount, concept, items, data, trx_id, status)
            VALUES ($1, $2, $3, $4, $5, $6, $7)",
        [SellerId, Amount, Concept, Items, DataStr, TrxId, Status]
    ).

updatePayment(Conn, Data, TrxId, Status) ->
    DataStr = jsx:encode(Data),
    epgsql:equery(Conn,
        "UPDATE refundcheck.payment
	        SET data = $1, status = $2
	        WHERE trx_id = $3",
        [DataStr, Status, TrxId]
    ).

selectPayment(Conn, TrxId) ->
    RS = epgsql:equery(Conn,
        "SELECT date, seller_id, amount, concept, items, best_before, data, trx_id, status
        FROM refundcheck.payment
        WHERE trx_id = $1",
        [TrxId]
    ),
    [R] = parse_result(RS),
    R.
    



selectCustomerIds(Conn, CustomerMail) ->
    RS = epgsql:equery(Conn,
        "SELECT id FROM refundcheck.customer c WHERE c.mail = $1",
        [CustomerMail]
    ),
    getColValues(RS, id).


    
insertSeller(Conn, SellerName, SellerMail, SellerKey, SellerAvailCalls) ->
    epgsql:equery(Conn,
        "INSERT INTO refundcheck.seller (
            name, mail, key, available_calls)
            VALUES ($1, $2, $3, $4)",
        [SellerName, SellerMail, SellerKey, SellerAvailCalls]
    ).

% selectSellerLastPaymentDate(Conn, SellerId) ->
%     RS = epgsql:equery(Conn,
%         "SELECT max(date) d FROM refundcheck.payment p WHERE p.seller_id = $1",
%         [SellerId]
%     ),
%     getColValues(RS, d).


insertCustomer(Conn, CustomerMail) ->
    epgsql:equery(Conn,
        "INSERT INTO refundcheck.customer (
            mail)
            VALUES ($1)",
        [CustomerMail]
    ).


deleteCustomer(Conn, CustomerMail) ->
    epgsql:equery(Conn,
        "DELETE FROM refundcheck.customer
            WHERE mail = $1",
        [CustomerMail]
    ).



% insertPurchase(Conn, SellerId, CustomerId, ProductTypeId, Date, AmountRangeId, AmountCurrency, TransactionId) ->
%     epgsql:equery(Conn,
%         "INSERT INTO refundcheck.purchase (
%             seller_id, customer_id, product_type_id, date, 
%             amount_range_id, amount_currency, transaction_id)
%             VALUES (?, ?, ?, ?, ?, ?, ?)",
%         [SellerId, CustomerId, ProductTypeId, Date, AmountRangeId, AmountCurrency, TransactionId]
%     ).

insertPurchase(Conn, SellerId, CustomerId, ProductTypeId, AmountRangeId, AmountCurrency, TransactionId) ->
    epgsql:equery(Conn,
        "INSERT INTO refundcheck.purchase (
            seller_id, customer_id, product_type_id, 
            amount_range_id, amount_currency, transaction_id)
            VALUES ($1, $2, $3, $4, $5, $6)",
        [SellerId, CustomerId, ProductTypeId, AmountRangeId, AmountCurrency, TransactionId]
    ).

deletePurchase(Conn, SellerId, CustomerId) ->
    epgsql:equery(Conn,
        "DELETE FROM refundcheck.purchase
            WHERE seller_id = $1
            AND customer_id = $2",
        [SellerId, CustomerId]
    ).

selectPurchasesByCustomerAndSeller(Conn, CustomerId, []) ->
    RS = epgsql:equery(Conn,
        "SELECT date, seller_id, customer_id, product_type_id, 
            amount_range_id, amount_currency, transaction_id
        FROM refundcheck.purchase p WHERE p.customer_id = $1",
        [CustomerId]
    ),
    R = parse_result(RS),
    R;
selectPurchasesByCustomerAndSeller(Conn, CustomerId, SellerId) ->
    RS = epgsql:equery(Conn,
        "SELECT date, seller_id, customer_id, product_type_id, 
            amount_range_id, amount_currency, transaction_id
        FROM refundcheck.purchase p
        WHERE p.customer_id = $1
        AND   p.seller_id = $2",
        [CustomerId, SellerId]
    ),
    R = parse_result(RS),
    R.


%% -spec Purchases :: list(map())
selectRefundsByPurchases(Conn, Purchases) when is_list(Purchases) ->
    lists:foldl(
        fun(
            #{
                <<"seller_id">>      := SellerId,
                <<"transaction_id">> := PurchaseTrxId
            },
            RefundsSoFar
        ) ->
            R = selectRefunds(Conn, SellerId, PurchaseTrxId),
            lists:append(RefundsSoFar, R)
        end,
        [],
        Purchases
    ).


selectRefunds(Conn, SellerId, PurchaseTrxId) ->
    RS = epgsql:equery(Conn,
        "SELECT date, seller_id, type_id, 
            description, purchase_transaction_id
        FROM refundcheck.refund r
        WHERE r.seller_id = $1
        AND   r.purchase_transaction_id = $2",
        [SellerId, PurchaseTrxId]
    ),
    R = parse_result(RS),
    R.



insertRefund(Conn, SellerId, PurchaseTrxId, RefundTypeId, RefundDescription) ->
    epgsql:equery(Conn,
        "INSERT INTO refundcheck.refund(
            seller_id, purchase_transaction_id, type_id, description)
            VALUES ($1, $2, $3, $4)",
        [SellerId, PurchaseTrxId, RefundTypeId, RefundDescription]
    ).


consolidateRefundInfo([], Refunds) ->
    RefundsNum = length(Refunds),
    {Risk, Description} = getRefundRiskAndDescription(0, 0),
    #{
        result        => <<"ok">>,
        description   => Description,
        purchases_num => 0,
        refunds_num   => RefundsNum,
        refunds_p     => 0,
        risk          => Risk
    };
consolidateRefundInfo(Purchases, Refunds) ->
    PurchasesNum = length(Purchases),
    RefundsNum   = length(Refunds),
    RefundsP     = (100 * RefundsNum) div PurchasesNum,
    {Risk, Description} = getRefundRiskAndDescription(PurchasesNum, RefundsP),
    #{
        result        => <<"ok">>,
        description   => Description,
        purchases_num => PurchasesNum,
        refunds_num   => RefundsNum,
        refunds_p     => RefundsP,
        risk          => Risk
    }.



getRefundRiskAndDescription(0, 0) ->
    {0, <<"no data for this customer">>};
getRefundRiskAndDescription(PurchasesNum, 0) when PurchasesNum < 3 ->
    {0, <<"very few data for the customer, but looking good">>};
getRefundRiskAndDescription(_, 0) ->
    {0, <<"customer looking great">>};
getRefundRiskAndDescription(_, RefundsP) when RefundsP < 30 ->
    {10, <<"customer has some refunds, looking fair">>};
getRefundRiskAndDescription(_, RefundsP) when RefundsP < 51 ->
    {30, <<"customer looks suspicious">>};
getRefundRiskAndDescription(_, _) ->
    {90, <<"customer looks abusive">>}.



%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parse_result({error, #error{ code = <<"23505">>, extra = Extra }}) ->
    {match, [Column]} =
        re:run(proplists:get_value(detail, Extra),
               "Key \\(([^\\)]+)\\)", [{capture, all_but_first, binary}]),
    throw({error, {non_unique, Column}});
parse_result({error, #error{ message = Msg }}) ->
    throw({error, Msg});
parse_result({ok, Cols, Rows}) ->
    to_map(Cols, Rows);
parse_result({ok, Counts, Cols, Rows}) ->
    {ok, Counts, to_map(Cols, Rows)};
parse_result(Result) ->
    Result.


to_map(Cols, Rows) ->
    [ maps:from_list(lists:zipwith(fun(#column{name = N}, V) -> {N, V} end,
                                        Cols, tuple_to_list(Row))) || Row <- Rows ].

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

