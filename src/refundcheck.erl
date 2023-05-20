-module(refundcheck).

-include_lib("epgsql/include/epgsql.hrl").

% -feature(maybe_expr, enable). 

-export([
    getColValues/2,
    registerPurchase/1,
    registerRefund/1,
    getCustomerHistory/1
]).

-compile([export_all]).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

registerPurchase(PurchaseData) ->

    %TODO validatePurchaseData(PurchaseData)

    #{
        seller_mail     := SellerMail,
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
    ok = epgsql:close(Conn),
      
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
    

registerRefund(RefundData) ->
    #{
        seller_mail := SellerMail,
        purchase_trx_id := PurchaseTrxId,
        refund_type := RefundTypeId,
        extra_info := RefundDescription
    } = RefundData,
    
    Conn = getConnection(),

    [SellerId] = selectSellerIds(Conn, SellerMail),
    Res = insertRefund(Conn, SellerId, PurchaseTrxId, RefundTypeId, RefundDescription),
    io:format("Insert Res:~p~n", [Res]),
    ok = epgsql:close(Conn),
      
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
    

getCustomerHistory(SellerId, CustomerMail) ->

    Conn = getConnection(),

    % get customer
    CustomerIds = selectCustomerIds(Conn, CustomerMail),
    CustomerId = case CustomerIds of
        [] ->
            insertCustomer(Conn, CustomerMail),
            [NewCId] = selectCustomerIds(Conn, CustomerMail),
            NewCId;
        [CId] -> CId
    end,

    % get customer's purchases history
    Purchases = selectPurchasesByCustomerAndSeller(Conn, CustomerId, SellerId),
    
    % get refunds history
    Refunds = selectRefundsByPurchases(Conn, Purchases),

    % consolidate refunds info
    % TODO
    Info = consolidateRefundInfo(Purchases, Refunds),
    Info.


getCustomerHistory(CustomerMail) ->

    Conn = getConnection(),

    % get customer
    CustomerIds = selectCustomerIds(Conn, CustomerMail),
    CustomerId = case CustomerIds of
        [] ->
            insertCustomer(Conn, CustomerMail),
            [NewCId] = selectCustomerIds(Conn, CustomerMail),
            NewCId;
        [CId] -> CId
    end,

    % get customer's purchases history
    Purchases = selectPurchasesByCustomer(Conn, CustomerId),
    
    % get refunds history
    Refunds = selectRefundsByPurchases(Conn, Purchases),

    % consolidate refunds info
    % TODO
    Info = consolidateRefundInfo(Purchases, Refunds),
    Info.


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

getConnection() ->
    Host = "localhost",
    Username = "cam",
    Password = "Casteril0!",  %TODO  ### WARNING!!  use anon fun insted of plain text for the pass
    {ok, Conn} = epgsql:connect(Host, Username, Password, []),
    Conn.


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

selectCustomerIds(Conn, CustomerMail) ->
    RS = epgsql:equery(Conn,
        "SELECT id FROM refundcheck.customer c WHERE c.mail = $1",
        [CustomerMail]
    ),
    getColValues(RS, id).


    
insertSeller(Conn, SellerName, SellerMail, SellerPass) ->
    epgsql:equery(Conn,
        "INSERT INTO refundcheck.seller (
            name, mail, pass)
            VALUES ($1, $2, $3)",
        [SellerName, SellerMail, SellerPass]
    ).

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

selectPurchasesByCustomer(Conn, CustomerId) ->
    RS = epgsql:equery(Conn,
        "SELECT date, seller_id, customer_id, product_type_id, 
            amount_range_id, amount_currency, transaction_id
        FROM refundcheck.purchase p WHERE p.customer_id = $1",
        [CustomerId]
    ),
    R = parse_result(RS),
    R.


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


consolidateRefundInfo(Purchases, Refunds) ->
    PurchasesNum = length(Purchases),
    RefundsNum   = length(Refunds),
    RefundsP     = (100 * RefundsNum) div PurchasesNum,
    Description  = getRefundInfoDescription(PurchasesNum, RefundsP),
    #{
        description   => Description,
        purchases_num => PurchasesNum,
        refunds_num   => RefundsNum,
        refunds_p     => RefundsP
    }.



getRefundInfoDescription(0, 0) ->
    "no data for this customer";
getRefundInfoDescription(PurchasesNum, 0) when PurchasesNum < 3 ->
    "very few data for the customer, but looking good";
getRefundInfoDescription(_, 0) ->
    "customer looking great";
getRefundInfoDescription(_, RefundsP) when RefundsP < 30 ->
    "customer has some refunds, looking fair";
getRefundInfoDescription(_, RefundsP) when RefundsP < 51 ->
    "customer looks suspicious";
getRefundInfoDescription(_, _) ->
    "customer looks abusive".



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

