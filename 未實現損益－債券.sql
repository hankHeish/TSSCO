
declare @sDate datetime 
set @sDate =
(
	select Top 1 Tradingdate
	from [DBMain].[dbo].[Tradingdate]
	where MONTH(Tradingdate) = (case
									when  MONTH(CONVERT(datetime, GETDATE(), 105)) = 1 then 12
									else MONTH(CONVERT(datetime, GETDATE(), 105)) - 1
								end )
		and YEAR(Tradingdate) = (case 
									when MONTH(CONVERT(datetime, GETDATE(), 105)) = 1 then YEAR(CONVERT(datetime, GETDATE(), 105)) - 1
									else YEAR(CONVERT(datetime, GETDATE(), 105))
								 end )
	order by Tradingdate desc 
)

create table #AcctPortf(Acc nvarchar(50), Portfolio nvarchar(50))
insert #AcctPortf
SELECT distinct(Acc)
	  , Portfolio
FROM [DBMain].[dbo].[DefaultPortfolio_債券部]
where BDate <= @sDate and EDate >= @sDate

--select * from #AcctPortf
--declare @Date1 datetime, @Date2 datetime
--set @Date1 = '2017-07-31'
--set @Date2 = '20170731'
SELECT *
	  ,FutPL + StkPL + LoanPL as CumPL
FROM
(
	SELECT StockID
		  ,FutID
		  ,LoanID
		  ,case 
		       when FutPL.Portfolio is not NULL then FutPL.Portfolio
			   when StkPL.Portfolio is not NULL then StkPL.Portfolio
			   when Loan.Portfolio is not NULL then Loan.Portfolio
		   end as Portfolio 
		  ,case 
			   when nReal_Fut_CmuPL is NULL then 0
			   else nReal_Fut_CmuPL
		   end as FutPL
		  ,case 
			   when nReal_Stk_CumPL is NULL then 0
			   else nReal_Stk_CumPL
		   end as StkPL
		  ,case 
			   when Loan_CumPL is NULL then 0
			   else Loan_CumPL
		   end as LoanPL
	FROM
	(
		--Stock PL
		select distinct(交易員代碼) as StockID
				,Portfolio
				,SUM(今日未實現損益) over(partition by 交易員代碼 order by 交易員代碼) as nReal_Stk_CumPL
		from [PL].[dbo].[400現股庫存檔wkstkx] as Stock
		join #AcctPortf as Portfolio on(Stock.交易員代碼 = Portfolio.Acc)
		--where 庫存日期 = @Date2
		where 庫存日期 = @sDate
	)as StkPL
	full join
	(
		--Futures PL
		select distinct(交易代號) as FutID
			  ,Portfolio
			  ,SUM(未平倉損益) over(partition by 交易代號 order by 交易代號) as nReal_Fut_CmuPL
		from [PL].[dbo].[期貨_未平倉資料] as Acct_Fut
		join #AcctPortf as Acct on(Acct_Fut.交易代號 = Acct.Acc)
		--where 匯入日期 = @Date1
		where 匯入日期 = @sDate
	)as FutPL on(StkPL.Portfolio = FutPL.Portfolio)
	full join
	(
		--Loan PL
		select distinct(交易員代碼) as LoanID
			  ,Portfolio
			  ,SUM(今日未實現損益) over (partition by 交易員代碼 order by 交易員代碼) as Loan_CumPL
		from [PL].[dbo].[400券賣庫存損益檔wklnkx] as LoanPos
		join #AcctPortf as Portfolio on (Portfolio.Acc = LoanPos.交易員代碼)
		where 庫存日期 = @sDate
	)as Loan on (Loan.LoanID = StkPL.StockID)

)as TotalPL
order by TotalPL.StockID



drop table #AcctPortf

