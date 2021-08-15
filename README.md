coinTrader
===========

This library will help you to connect to a particular trading API. At the current moment, it will only connect to Indodax but it will help you to connect to another platform ASAP.

## INSTALL

To install the library, you can use devtools as follow.

```
devtools::install_github("blakcjack/coinTrader")
```

Note: This library is not a robot trading, it is just a client to help you execute some trading command. It will only help you build your own robot trading.

## DEPENDENCIES

This library depends on some packages as follow:

- R6
- digest
- stringr
- tidyr
- Rcpp

## USAGE EXAMPLE

### Initialize the client

If you don't want to provide your key and secret, it is recommended to leave the secret and key blank. To provide them, you just need to provide them through your .Renviron. You can save them in format: `api_key_indodax = your_key` for key,  `secret_key_indodax = your_secret` for secret, and even `url_api_indodax = indodax_private_api_url` for url.

```
library(coinTrader)
client <- indodax$new()
#or
client <- indodax$new(url = url, secret = secret, key = key)
#or
client <- indodax$new(url = url)
```

### Get Aset Information

```
aset_information <- client$assetInfo()
```

### Execute Buy Command

```
client$buy(
  pair = "eth_idr",
  idr = 1000000
)
```

Take a look at the idr, it is just price that indicates how much you will spend your money to buy the corresponding coin.

### Execute Sell Command

```
client$sell(
  pair = "eth_idr",
  val = 0.0119
)
```

This command will get error if you put the value greater than your balance.
