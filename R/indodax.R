#' @name Indodax
#' @aliases Indodax
#' @title API for Trading at Indodax
#' @author Suberlin Sinaga (2021)
#' @param url indodax api endpoint
#' @param secret indodax secret API
#' @param key indodax key API
#' @description
#' Connect to Indodax API endpoints
#' @details
#' Help you connect to Indodax API endpoints and execute your trading function.
#' 
#' Note: this is not a robot trading. This only helps you connect and execute your trading command.
#' @usage
#' indodax$new()
#' indodax$new(url, key, secret)
#'
#' @seealso
#' \link{httr} for API management in R
#'
#' @export
indodax <- R6Class(
    classname = "Indodax",
    
    private = list(
        signature = function(params) {
            hmac(eval(private$secret),
                 params, "sha512")
        },
        
    nonce = function() {
            as.numeric(Sys.time()) * 1000
        }
    ),
    
    public = list(
        #' @description Initializing new object
        #' @param url indodax api endpoint
        #' @param secret indodax secret API
        #' @param key indodax key API
        #' @details
        #' To initialize this object, you need to call `new()` method
        #' that will span this initialize function. All the arguments
        #' are optional.
        #' 
        #' When not supplied, you need to store them in you .Renviron as:
        #'       api_key_indodax for key
        #'       secret_key_indodax for secret
        #'       url_api_indodax for url
        initialize = function(url = NULL, secret = NULL, key = NULL) {
            if (!missing(secret) & !missing(key)) {
                cat(
                    "Creating Indodax object using supplied key! Please consider about the vulnerability of this method.\n"
                )
                private$secret = secret
                private$key = key
            } else {
                cat("Creating Indodax object using environment key. This is the recommended way.\n")
                private$key <- quote(Sys.getenv("api_key_indodax"))
                private$secret <-
                    quote(Sys.getenv("secret_key_indodax"))
                private$url <- quote(Sys.getenv("url_api_indodax"))
            }
            if (!missing(url)) {
                private$url <- url
            }
        },
        
        #' @details Representation to print the object
        print = function() {
            cat("<Indodax API bridge>")
        },
        
        #' @param payload The post data that will be sent
        #' @param params The post data in url-encoded form
        #' @details
        #' Posting the data to the endpoint to get a response
        #' @return
        #' Fetched object of the Indodax API
        posting = function(payload, params) {
            sign <- private$signature(params)
            res <- POST(
                url = eval(private$url),
                body = payload,
                add_headers(`key` = eval(private$key),
                            `sign` = sign),
                httr::content_type('application/x-www-form-urlencoded'),
                encode = "form"
            )
            return(content(res))
        },
        
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return
        #' dataframe of balance and held balance
        #' @details
        #' Gives us information about our current assets. It will give us the information about the current balance and current held balance
        assetInfo = function(timestamp = 1578304294000,
                             recvWindow = 1578303937000) {
            
            params <- paste0('method=getInfo&timestamp=',
                             timestamp,
                             '&recvWindow=',
                             recvWindow)
            
            payload <- list(method = "getInfo",
                            timestamp = timestamp, recvWindow = recvWindow)
            res <- self$posting(payload, params)
            # get_normal_balance
            res_balance <- res$return$balance %>%
                bind_cols() %>%
                mutate_all(as.numeric) %>%
                pivot_longer(everything(), names_to = "aset") %>%
                filter(value != 0) %>%
                mutate(on_hold = 0)
            # ge both normal and hold balance
            res <- res$return$balance_hold %>%
                bind_rows() %>%
                mutate_all(as.numeric) %>%
                pivot_longer(everything(), names_to = "aset") %>%
                filter(value != 0) %>%
                mutate(on_hold = 1) %>%
                rbind(res_balance) %>%
                mutate(last_retrieve = Sys.time())
            return(res)
        },
        
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @details
        #' Fetch transaction history from Indodax server
        #' @return
        #' list of transaction history
        transHistory = function(timestamp = 1578304294000,
                                recvWindow = 1578303937000,
                                since = "2021-03-28 00:00:00") {
            since <- as.numeric(as.POSIXct(since, tz = "Asia/Jakarta",
                                           origin = "1970-01-01"))
            params <- paste0('method=transHistory&timestamp=', timestamp,
                             '&recvWindow=', recvWindow, '&since=', since)
            payload <-
                list(method = "transHistory",
                     timestamp = timestamp,
                     recvWindow = recvWindow,
                     since = since)
            res <- self$posting(payload, params)
            if (res$success == 1) {
                cat("Fetched!\n")
                res <- res$return
            } else {
                res <- res$error
                class(res) <- c(oldClass(res), "errorAPI")
            }
            return(res)
        },
        
        #' @details
        #' Gives us the withdrawal history
        #' @return
        #' Data frame of withdrawal history
        withdrawHistory = function() {
            res <- self$transHistory()$withdraw
            df_res <- list()
            for (a in seq_len(length(res))) {
                if (length(res[[a]]) > 0) {
                    df_res[[a]] <- res[[a]] %>%
                        bind_cols() %>%
                        mutate(source = names(res[a]))
                }
            }
            df_res <- df_res %>%
                bind_rows()
            
            if (length(df_res) > 8) {
                df_res <- df_res %>%
                    select(1:7,source, everything()) %>%
                    pivot_longer(cols = 9:last_col()) %>%
                    filter(!is.na(value)) %>%
                    select(-name) %>% 
                    distinct()
            }
            
            return(df_res)
        },
        
        #' @details
        #' Gives us the deposit history
        #' @return
        #' Data frame of deposit history
        depositHistory = function() {
            res <- self$transHistory()$deposit
            df_res <- list()
            for (a in seq_len(length(res))) {
                if (length(res[[a]]) > 0) {
                    df_res[[a]] <- res[[a]] %>%
                        bind_rows() %>%
                        mutate(source = names(res[a]))
                }
            }
            df_res <- df_res %>%
                bind_rows()
            
            if (length(df_res) > 10) {
                df_res <- df_res %>%
                    pivot_longer(cols = 11:last_col()) %>%
                    select(-name) %>%
                    filter(!is.na(value) | !is.na(type)) %>%
                    distinct()
            }
            
            return(df_res)
        },
        
        #' @details
        #' Gives us the trading history of corresponding coin. We can grab as many as coins we are interested in by passing them as vector.
        #' @param pairs define specific pairs we want to watch. It supports the vector of pairs
        #' @param count number of trading history of each interested coin
        #' @param from_id trading id we want to watch from
        #' @param end_id the last trading id we want to watch
        #' @param order the order of the record
        #' @param since specifies the interested time frame of the history
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return
        #' Raw content of the fetched API object
        tradeHistory = function(pairs = 'eth_idr', count = 1000, from_id = 0,
                                end_id = 0, order = "desc",
                                since = "2021-03-28 00:00:00",
                                timestamp = 1578304294000,
                                recvWindow = 1578303937000) {
            since <- as.numeric(as.POSIXct(since, tz = "Asia/Jakarta",
                                           origin = "1970-01-01"))
            if (length(pairs) > 1) {
                res <- lapply(pairs, function(pair) {
                    params <- paste0(
                        'method=tradeHistory&timestamp=', timestamp,
                        '&recvWindow=', recvWindow,
                        '&count=', count,
                        '&from_id=', from_id,
                        '&end_id=', end_id,
                        '&order=', order,
                        '&since=', since,
                        '&pair=', pair
                    )
                    payload <-
                        list(
                            method = "tradeHistory",
                            timestamp = timestamp,
                            recvWindow = recvWindow,
                            count = count,
                            from_id = from_id,
                            end_id = end_id,
                            order = order,
                            since = since,
                            pair = pair
                        )
                    self$posting(payload, params)
                }) %>%
                    bind_rows() %>%
                    filter(success == 1)
                res <- res$return %>%
                    bind_rows()
                res <- res %>%
                    select(currency,
                           pair,
                           trade_id,
                           order_id,
                           type,
                           price,
                           fee,
                           trade_time,
                           everything()) %>%
                    pivot_longer(cols = 9:length(res)) %>%
                    filter(!is.na(value)) %>%
                    select(-name)
            } else {
                params <- paste0(
                    'method=tradeHistory&timestamp=',  timestamp,
                    '&recvWindow=', recvWindow,
                    '&count=', count,
                    '&from_id=', from_id,
                    '&end_id=', end_id,
                    '&order=', order,
                    '&since=', since,
                    '&pair=', pairs
                )
                payload <-
                    list(
                        method = "tradeHistory", timestamp = timestamp,
                        recvWindow = recvWindow, count = count,
                        from_id = from_id, end_id = end_id,
                        order = order, since = since, pair = pairs
                    )
                res <-
                    self$posting(payload, params)$return$trades %>%
                    bind_rows() %>%
                    select(currency,
                           pair,
                           trade_id,
                           order_id,
                           type,
                           price,
                           fee,
                           trade_time,
                           everything())
                names(res)[length(res)] <- "value"
            }
            return(res)
        },
        
        #' @details
        #' Gives us the buy history of corresponding coin. We can grab as many as coins we are interested in by passing them as vector.
        #' @param pairs define specific pairs we want to watch. It supports the vector of pairs
        #' @param count number of trading history of each interested coin
        #' @param from_id trading id we want to watch from
        #' @param end_id the last trading id we want to watch
        #' @param order the order of the record
        #' @param since specifies the interested time frame of the history
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return
        #' data.frame
        buyHistory = function(pairs = 'eth_idr', count = 1000,
                              from_id = 0, end_id = 0,
                              order = "desc", since = "2021-03-28 00:00:00",
                              timestamp = 1578304294000,
                              recvWindow = 1578303937000) {
            res <-
                self$tradeHistory(pairs,
                                  count,
                                  from_id,
                                  end_id,
                                  order,
                                  since,
                                  timestamp,
                                  recvWindow) %>%
                filter(type == "buy") %>%
                mutate_at(vars(price, fee, value, trade_time), as.numeric)
            return(res)
        },
        
        #' @details
        #' Gives us the buy history of corresponding coin. We can grab as many as coins we are interested in by passing them as vector.
        #' @param pairs define specific pairs we want to watch. It supports the vector of pairs
        #' @param count number of trading history of each interested coin
        #' @param from_id trading id we want to watch from
        #' @param end_id the last trading id we want to watch
        #' @param order the order of the record
        #' @param since specifies the interested time frame of the history
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return
        #' data.frame
        sellHistory = function(pairs = 'eth_idr', count = 1000,
                               from_id = 0, end_id = 0,
                               order = "desc", since = "2021-03-28 00:00:00",
                               timestamp = 1578304294000,
                               recvWindow = 1578303937000) {
            res <-
                self$tradeHistory(pairs, count, from_id, end_id, order,
                                  since, timestamp, recvWindow) %>%
                filter(type == "sell") %>%
                mutate_at(vars(price, fee, value, trade_time), as.numeric)
            
            return(res)
        },
        
        #' @details
        #' Function to execute buy and sell. Do not directly execute the function. Use `sell()` or `buy()`.
        #' @param pair define specific pairs we want to watch. It supports the vector of pairs
        #' @param type type of command to execute (sell or buy)
        #' @param price at price should the command will be executed
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @param idr amount of rupiah(s) that we want to spend to buy the coin(s)
        #' @param val amount of coin(s) that we want to sell
        #' @return
        #' raw data from server
        trade = function(pair, type, price, timestamp = 1578304294000,
                         recvWindow = 1578303937000, idr = NULL, val = NULL) {
            if (type == "sell" & (is.null(val) | missing(price))) {
                stop("Please specify the amount of coins that need to be sold.")
            }
            if (type == "sell" & !is.null(val) & !missing(price)) {
                params <- paste0(
                    'method=trade&timestamp=', timestamp,
                    '&recvWindow=', recvWindow,
                    '&pair=', pair,
                    '&type=', type,
                    '&price=', price,
                    '&', gsub("_idr", "", pair), '=', val
                )
                payload <-
                    list(
                        method = "trade",
                        timestamp = timestamp,
                        recvWindow = recvWindow,
                        pair = pair,
                        type = type,
                        price = price,
                        val = val
                    )
                names(payload)[8] <- gsub("_idr", "", pair)
            }
            if (type == "buy" & (is.null(idr) | missing(price))) {
                stop("Please sepcify the amount of rupiah that will be used to buy.")
            }
            if (type == "buy" & !is.null(idr) & !missing(price)) {
                params <- paste0(
                    'method=trade&timestamp=',
                    timestamp,
                    '&recvWindow=', recvWindow,
                    '&pair=', pair,
                    '&type=', type,
                    '&price=', price,
                    '&idr=', idr
                )
                payload <-
                    list(
                        method = "trade",
                        timestamp = timestamp,
                        recvWindow = recvWindow,
                        pair = pair,
                        type = type,
                        price = price,
                        idr = idr
                    )
            }
            if (missing(pair)) {
                stop("Please specify the current pair conversion target.")
            }
            res <- self$posting(payload, params)
            return(res)
        },
        
        #' @details
        #' Execute sell command.
        #' @param pair define specific pairs we want to watch. It supports the vector of pairs
        #' @param val amount of coin(s) that we want to sell
        #' @return
        #' data_frame
        sell = function(pair, val, ...) {
            price <- self$current_price(pair)$buy
            res <-
                self$trade(
                    type = "sell",
                    pair = pair,
                    price = price,
                    val = val,
                    ...
                )
            if (res$success == 1) {
                cat("Sold!\n")
                res <- res$return %>%
                    bind_rows()
            } else {
                res <- res$error
            }
            return(res)
        },
        
        #' @details
        #' Execute buy command.
        #' @param pair define specific pairs we want to watch. It supports the vector of pairs
        #' @param idr amount of rupiah(s) that we want to spend to buy the coin(s)
        #' @return
        #' data_frame
        buy = function(pair, idr, ...) {
            price <- self$current_price(pair)$sell
            
            res <-
                self$trade(
                    type = "buy",
                    pair = pair,
                    price = price,
                    idr = idr,
                    ...
                )
            
            if (res$success == 1) {
                cat("Bought!\n")
                res <- res$return %>%
                    bind_rows()
            } else {
                res <- res$error
            }
            
            return(res)
        },
        
        #' @details 
        #' Execute command to open order position
        #' @param pair define specific pairs we want to watch. It supports the vector of pairs
        #' @param timestamp  the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return raw content of the function
        openOrders = function(pair = NULL,
                              timestamp = 1578304294000,
                              recvWindow = 1578303937000) {
            if (!is.null(pair)) {
                params <- paste0(
                    'method=openOrders&timestamp=', timestamp,
                    '&recvWindow=', recvWindow,
                    "&pair=", pair
                )
                payload <-
                    list(
                        method = "openOrders",
                        timestamp = timestamp,
                        recvWindow = recvWindow,
                        pair = pair
                    )
            } else {
                params <- paste0('method=openOrders&timestamp=', timestamp,
                                 '&recvWindow=', recvWindow)
                payload <- list(method = "openOrders",
                                timestamp = timestamp,
                                recvWindow = recvWindow)
            }
            
            res <- self$posting(payload, params)
            if (res$success == 1) {
                cat("Fetched!\n")
                if (!is.null(pair)) {
                    res <- res$return$orders
                    for (i in length(res)) {
                        a[[i]] %>%
                            bind_rows() %>%
                            mutate(coin = names(a[[i]]))
                    }
                } else {
                    res <- res$return$orders
                    res <- lapply(res, function(a) {
                        a %>%
                            bind_rows() %>%
                            mutate(coin = pair)
                    })
                }
                
            } else {
                res <- res$error
                class(res) <- c(oldClass(res), "errorAPI")
            }
            return(res)
        },
        
        #' @details 
        #' Get the details order history opened
        #' @param pairs define specific pairs we want to watch. It supports the vector of pairs
        #' @param count number of trading history of each interested coin
        #' @param from order id we want to watch from
        #' @param timestamp the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return raw content of the function
        orderHistory = function(pairs, count = 1000,
                                from = 0, timestamp = 1578304294000,
                                recvWindow = 1578303937000) {
            if (length(pairs) > 1) {
                res <- lapply(pairs, function(pair) {
                    params <- paste0(
                        'method=orderHistory&timestamp=', timestamp,
                        '&recvWindow=', recvWindow,
                        '&count=', count,
                        '&from=', from,
                        '&pair=', pair
                    )
                    payload <-
                        list(
                            method = "orderHistory",
                            timestamp = timestamp,
                            recvWindow = recvWindow,
                            count = count,
                            from = from,
                            pair = pair
                        )
                    self$posting(payload, params)
                }) %>%
                    bind_rows() %>%
                    filter(success == 1)
                res <- res$return %>%
                    bind_rows() %>%
                    select(-contains("remain")) %>%
                    select(order_id,
                           type,
                           price,
                           submit_time,
                           finish_time,
                           status,
                           everything()) %>%
                    pivot_longer(cols = 7:last_col()) %>%
                    filter(!is.na(value)) %>%
                    mutate(name = str_replace_all(name, "order_", ""))
            } else {
                params <- paste0(
                    'method=orderHistory&timestamp=',
                    timestamp,
                    '&recvWindow=',
                    recvWindow,
                    '&count=',
                    count,
                    '&from=',
                    from,
                    '&pair=',
                    pairs
                )
                payload <-
                    list(
                        method = "orderHistory",
                        timestamp = timestamp,
                        recvWindow = recvWindow,
                        count = count,
                        from = from,
                        pair = pairs
                    )
                res <-
                    self$posting(payload, params)$return$orders %>%
                    bind_rows() %>%
                    select(-contains("remain")) %>%
                    select(order_id,
                           type,
                           price,
                           submit_time,
                           finish_time,
                           status,
                           everything()) %>%
                    pivot_longer(cols = 7:last_col()) %>%
                    filter(!is.na(value)) %>%
                    mutate(name = str_replace_all(name, "order_", ""))
            }
            return(res)
        },
        
        #' @details 
        #' Get the details of a specific order
        #' @param pair define specific pair we want to watch. It does not support the vector of pair.
        #' @param order_id order_id that we want to check the detail of
        #' @param timestamp the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return data.frame
        getOrder = function(pair, order_id,
                            timestamp = 1578304294000,
                            recvWindow = 1578303937000) {
            params <- paste0(
                'method=getOrder&timestamp=', timestamp,
                '&recvWindow=', recvWindow,
                '&order_id=', order_id,
                '&pair=', pair
            )
            payload <-
                list(
                    method = "getOrder",
                    timestamp = timestamp,
                    recvWindow = recvWindow,
                    order_id = order_id,
                    pair = pair
                )
            res <- self$posting(payload, params)$return$order %>%
                bind_rows()
            return(res)
        },
        
        #' @details 
        #' Cancel specific order id
        #' @param pair define specific pair we want to watch. It does not support the vector of pair.
        #' @param order_id order_id that we want to check the detail of
        #' @param type order type that we want to cancel (buy or sell)
        #' @param timestamp the millisecond timestamp of when the request was created and sent
        #' @param recvWindow the number of millisecond after timestamp where your request is valid
        #' @return data.frame
        cancelOrder = function(pair,
                               order_id,
                               type,
                               timestamp = 1578304294000,
                               recvWindow = 1578303937000) {
            params <- paste0(
                'method=getOrder&timestamp=', timestamp,
                '&recvWindow=', recvWindow,
                '&order_id=', order_id,
                '&type=', type,
                '&pair=', pair
            )
            payload <-
                list(
                    method = "getOrder",
                    timestamp = timestamp,
                    recvWindow = recvWindow,
                    order_id = order_id,
                    type = type,
                    pair = pair
                )
            self$posting(payload, params)$return
            invisible(self)
        }
    ),
    lock_objects = FALSE
)
