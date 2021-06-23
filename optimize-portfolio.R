library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)


tickers <- c("VALE3.SA", "ITUB4.SA", "PETR4.SA", "TAEE11.SA", "B3SA3.SA", "VVAR3.SA", "EQTL3.SA", "KLBN11.SA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2015-06-22', periodicity = 'daily', auto.assign=FALSE)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min=.10, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev", target=0.010)

rp <- random_portfolios(portf, 10000, "sample")

opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            optimize_method="random",
                                            rp=rp,
                                            rebalance_on="months",
                                            training_period=5,
                                            rolling_window=5)

equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

ibovprices <- getSymbols.yahoo("^BVSP", from='2015-06-22', periodicity = 'daily', auto.assign=FALSE)[,4]
ibov <- na.omit(ROC(ibovprices))
ibov <- as.xts(ibov)

chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")

rebal_weights <-extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)

rets_df <- cbind(rebal_returns, benchmark, ibov)

charts.PerformanceSummary(rets_df, main="P/L Over Time")