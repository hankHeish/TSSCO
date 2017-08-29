 
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
--print @sDate

create table #AcctPortf(Acc nvarchar(50), Portfolio nvarchar(50))
insert #AcctPortf
SELECT distinct(Acc)
	  , Portfolio
FROM [DBMain].[dbo].[DefaultPortfolio_���f���]
where BDate <= @sDate and EDate >= @sDate

--select * from #AcctPortf

--declare @Date1 datetime, @Date2 datetime
--set @Date1 = '2017-07-31'
--set @Date2 = '20170731'
SELECT *
	  , Stk_PL + Fut_PL + Loan_PL as CumPL
FROM
(
	SELECT Stock.������N�X as StockID
		  , Future.����N�� as FutID
		  ,LoanID
		  ,	case 
		       when Future.Portfolio is not NULL then Future.Portfolio
			   when Stock.Portfolio is not NULL then Stock.Portfolio
			   when Loan.Portfolio is not NULL then Loan.Portfolio
		    end as Portfolio
		  , case 
				when Cum_Stk_PL is NULL then 0
				else Cum_Stk_PL
			end as Stk_PL
		  , case 
				when Cum_Fut_PL is NULL then 0
				else Cum_Fut_PL
			end as Fut_PL
		  , case 
				when Loan_CumPL is NULL then 0
				else Loan_CumPL
			end as Loan_PL
	FROM
	(
		--Stock PL
		SELECT distinct([������N�X])
			  ,Portfolio
			  ,SUM([���饼��{�l�q]) over(partition by ������N�X order by ������N�X) as Cum_Stk_PL
		FROM [PL].[dbo].[400�{�Ѯw�s��wkstkx] as Acct_Stock
		join #AcctPortf as Portfolio on(Acct_Stock.������N�X = Portfolio.Acc
										and Portfolio.Acc not in ('327', '6007'))
		--where �w�s��� = @Date2
		WHERE �w�s��� = @sDate
	)as Stock
	full join
	(
		--Futures PL
		SELECT distinct([����N��])
			  ,Portfolio
			  ,SUM([�����ܷl�q]) over(partition by ����N�� order by ����N��) as Cum_Fut_PL
		FROM [PL].[dbo].[���f_�����ܸ��] as Acct_Fut
		join #AcctPortf as Portfolio on(Acct_Fut.����N�� = Portfolio.Acc
										and Portfolio.Acc not in ('327', '6007'))
		--where �פJ��� = @Date1
		WHERE �פJ��� = @sDate
	)as Future on(Stock.Portfolio = Future.Portfolio)
	full join
	(
		--Loan PL
		select distinct(������N�X) as LoanID
			  ,Portfolio
			  ,SUM(���饼��{�l�q) over (partition by ������N�X order by ������N�X) as Loan_CumPL
		from [PL].[dbo].[400���w�s�l�q��wklnkx] as LoanPos
		join #AcctPortf as Portfolio on (Portfolio.Acc = LoanPos.������N�X)
		where �w�s��� = @sDate
	)as Loan on (Loan.LoanID = Stock.������N�X)
)as TotalPL
order by StockID

drop table #AcctPortf


