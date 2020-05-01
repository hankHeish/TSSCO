declare @t datetime set @t=(select max(日期) from cmoney.dbo.日收盤表排行)

create table #財報日(Q nvarchar(50),Y int,M nvarchar(50),D nvarchar(50))
insert #財報日
select '01',0,'5','15'
union
select '02',0,'8','31'
union
select '03',0,'11','15'
union
select '04',1,'3','31'

create table #取樣區間(年季 nvarchar(50),BDate datetime,EDate datetime)
insert #取樣區間
select x.年季,x.取樣日 as BDate,isnull(dbmain.dbo.tradingdateadd(-1,lead(x.取樣日) over(order by x.年季)),@t) as EDate
from (
	select cast(2000+ser as nvarchar(50))+b.Q as 年季,dbmain.dbo.tradingdateadd(0,cast(cast(2000+ser as int)+b.Y as nvarchar(50))+'/'+b.M+'/'+b.D) as 取樣日
	from dbmain.dbo.tradingdate a
	cross join #財報日 b
	where ser between 1 and 16
) x

create table #取樣(年季 nvarchar(50),股票代號 nvarchar(50),股票名稱 nvarchar(50)
,總名次 int,資本報酬率rnk int,盈餘殖利率rnk int,資本報酬率 decimal(20,10),盈餘殖利率 decimal(20,10))
insert #取樣
select x.年季,x.股票代號,x.股票名稱
,rank() over(partition by x.年季 order by x.資本報酬率rnk+x.盈餘殖利率rnk,x.資本報酬率+x.盈餘殖利率 desc)
 as 總名次
,x.資本報酬率rnk
,x.盈餘殖利率rnk
,x.資本報酬率,x.盈餘殖利率
from (
	select x.年季,x.股票代號,x.股票名稱
	,x.資本報酬率,x.盈餘殖利率
	,rank() over(partition by x.年季 order by x.資本報酬率 desc)
	 as 資本報酬率rnk
	,rank() over(partition by x.年季 order by x.盈餘殖利率 desc)
	 as 盈餘殖利率rnk
	from (
		select a.年季,a.股票代號,a.股票名稱
		,isnull(d.[營業利益(千)],0)/(isnull(a.[流動資產(千)],0)-isnull(a.[流動負債(千)],0)+isnull(a.[非流動資產(千)],0)-isnull(a.[無形資產(千)],0))
		 as 資本報酬率
		,isnull(d.[營業利益(千)],0)/(isnull(a.[普通股股本(千)],0)/10.*c.股價+isnull(a.[特別股股本(千)],0)+isnull(a.[非流動負債(千)],0)+isnull(a.[一年或一營業週期內到期長期負債(千)],0))
		 as 盈餘殖利率
		from cmoney.[dbo].[季IFRS財報(資產負債)] a
		join (
			select cast(datepart(yy,tradingdate) as nvarchar(50))+'0'+cast(datepart(qq,tradingdate) as nvarchar(50)) as 年季
			,max(tradingdate) as TDate
			from dbmain.dbo.tradingdate
			group by datepart(yy,tradingdate),datepart(qq,tradingdate)
		) b on b.年季=a.年季
		join testdb.[dbo].[大盤成份股每日市值] c on c.年月日=b.TDate and c.成份股=a.股票代號
		--join testdb.[dbo].[櫃買成份股每日市值] c on c.年月日=b.TDate and c.成份股=a.股票代號
		left join cmoney.[dbo].[季IFRS財報(損益單季)] d on d.年季=a.年季 and d.股票代號=a.股票代號
	) x
) x

select x.日期
,isnull(log( (x.IX/y.收盤價) / lag(x.IX/y.收盤價) over(order by x.日期) )
       ,x.ret-log(y.收盤價/(y.收盤價-y.漲跌))
	   )
 as RSret
,x.IX as MF30,y.收盤價 as 報酬指數
from (
	select x.年季,x.日期
	,100*exp(sum(x.ret) over(order by x.日期))
	 as IX
	,x.Ret,x.N
	from (
		select a.年季,c.日期
		,avg(case when c.收盤價-c.漲跌<>0 then log(c.收盤價/(c.收盤價-c.漲跌)) end)
		 as Ret
		,sum(case when c.股票代號 is not null then 1 else 0 end)
		 as N
		from #取樣 a
		join #取樣區間 b on b.年季=a.年季
		join cmoney.dbo.日收盤表排行 c on c.日期 between b.BDate and b.EDate and c.股票代號=a.股票代號
		where a.總名次<=30-- and c.收盤價-c.漲跌=0
		group by a.年季,c.日期
	) x
) x
left join cmoney.dbo.日收盤表排行 y on y.日期=x.日期 and y.股票代號='TWA02'
order by x.年季,x.日期

select a.年季,a.股票代號,a.股票名稱,a.總名次,a.資本報酬率rnk,a.盈餘殖利率rnk,a.資本報酬率,a.盈餘殖利率
,b.BDate,b.EDate
from #取樣 a
join #取樣區間 b on b.年季=a.年季
where a.總名次<=30 and a.年季>='200704'


drop table #取樣區間
drop table #財報日
drop table #取樣


