-module(refundcheck_test).

-include_lib("eunit/include/eunit.hrl").

t_test_() ->
    {setup,
        fun top_setup/0,
        fun top_cleanup/1,
        [
            {"db connection test", fun db_connection_t/0},

            {"register purchase white test", fun registerPurchase_t1/0},
            {"register purchase black test", fun registerPurchase_happy/0},
            {"register purchase with unreg customer", fun registerPurchase_newCustomer/0},

            {"register refund", fun registerRefund_happy/0}
        ]
    }.


top_setup() ->
    ?debugMsg("testing... starting appp"),
    application:ensure_all_started(refundcheck).

top_cleanup(_) ->
    ?debugMsg("top cleanup"),
    Conn = refundcheck:getConnection(),
    refundcheck:deletePurchase(Conn, 2, 1),

    CreatedCustMail = "new-customer@somestrangemail.com",
    CreatedCustIds = refundcheck:selectCustomerIds(Conn, CreatedCustMail),
    CreatedCustId  = hd(CreatedCustIds),
    refundcheck:deletePurchase(Conn, 2, CreatedCustId),
    refundcheck:deleteCustomer(Conn, CreatedCustMail),
    ok.

% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

db_connection_t() ->
    Host = "localhost",
    Username = "cam",
    Password = "Casteril0!",

    ?debugHere,
    X = epgsql:connect(Host, Username, Password, []),
    % ?debugFmt("epgsql:connect() : ~p", [X]),
    ?debugVal(X),
    
    ?assertMatch(
        {ok, _Conn},
        epgsql:connect(Host, Username, Password, []),
        "db connection test"
    ).



%% registerPurchase test, with a copy of the code for registerPurchase (something like white box)
registerPurchase_t1() ->

    SellerMail     = "seller.01@somemail.com",
    CustomerMail   = "customer.01@somemail.com",
    TransactionId  = "trx-" ++ integer_to_list(rand:uniform(1000000)),
    ProductTypeId  = 2,
    AmountRangeId  = "A",
    AmountCurrency = "USD",

    ?debugHere,
    Conn = refundcheck:getConnection(),

    SellerIds = refundcheck:selectSellerIds(Conn, SellerMail),
    SellerId  = hd(SellerIds),
    ?assertEqual(
        2,
        SellerId,
        "SellerId"
    ),
    CustomerIds = refundcheck:selectCustomerIds(Conn, CustomerMail),
    CustomerId  = hd(CustomerIds),
    ?assertEqual(
        1,
        CustomerId,
        "CustomerId"
    ),

    % insertPurchase(Conn, SellerId, CustomerId, ProductTypeId, Date, AmountRangeId, AmountCurrency, TransactionId),
    R = refundcheck:insertPurchase(Conn, SellerId, CustomerId, ProductTypeId, AmountRangeId, AmountCurrency, TransactionId),
    ?debugVal(R),

    ?assertEqual(
        {ok, 1},
        R,
        "registerPurchase test 1"
    ).


%% registerPurchase test - happy path
registerPurchase_happy() ->
    PurchaseData = #{
        seller_mail     => "seller.01@somemail.com",
        customer_mail   => "customer.01@somemail.com",
        trx_id          => "trx-" ++ integer_to_list(rand:uniform(1000000)),
        product_type    => 2,
        % date            => Date,
        amount_range    => "A",
        amount_currency => "USD"
    },

    R = refundcheck:registerPurchase(PurchaseData),

    ?assertEqual(
        ok,
        R,
        "registerPurchase test 2"
    ).


registerPurchase_newCustomer() ->
    CustomerMail = "new-customer@somestrangemail.com",
    PurchaseData = #{
        seller_mail     => "seller.01@somemail.com",
        customer_mail   => CustomerMail,
        trx_id          => "trx-" ++ integer_to_list(rand:uniform(1000000)),
        product_type    => 2,
        % date            => Date,
        amount_range    => "A",
        amount_currency => "USD"
    },

    R = refundcheck:registerPurchase(PurchaseData),

    ?assertEqual(
        ok,
        R,
        "registerPurchase test 2"
    ),
    
    Conn = refundcheck:getConnection(),
    RC = refundcheck:selectCustomerIds(Conn, CustomerMail),
    ?assertEqual(
        1,
        length(RC),
        "registerPurchase with previously unregistered customer"
    ).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


%% registerRefund test - happy path
registerRefund_happy() ->
    RefundData = #{
        seller_id  => 1,
        purchase_transaction_id => "t-1",
        type_id    => 1,
        description => "a descr of the refund"
    },

    R = refundcheck:registerRefund(RefundData),

    ?assertEqual(
        ok,
        R,
        "registerRefund test 1"
    ).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
