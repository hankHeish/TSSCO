
declare @t datetime set @t=(select max(日期) from cmoney.dbo.權證基本資料表每日) 
declare @y datetime set @y=dbmain.dbo.tradingdateadd(-1,@t)
declare @資料日期 datetime set @資料日期='2017/1/1'
--(select max(ResetDate) from [DBMain].[dbo].交易資產交換CBAS_Credit端部位限額)


declare @基本資料 table(股票代號 nvarchar(50),股票名稱 nvarchar(50),上市上櫃 nvarchar(50),[統一編號] nvarchar(50))
insert @基本資料
select 股票代號,股票名稱,上市上櫃,case when 股票代號='4991' then '53008500' else 統一編號 end
from cmoney.[dbo].[上市櫃公司基本資料]
insert @基本資料
select 代號,名稱,上市上櫃,[基金統編]
from cmoney.[dbo].[ETF基本資料]
insert @基本資料
select 代號,名稱,3,[統一編號]
from cmoney.[dbo].[興櫃基本資料] where 年度=(select max(年度) from cmoney.[dbo].[興櫃基本資料])
insert @基本資料
select 代號,名稱,4,[統一編號]
from cmoney.[dbo].[公開發行基本資料] where 年度=(select max(年度) from cmoney.[dbo].[公開發行基本資料])


insert @基本資料
select '000'+a.代號,a.名稱,5,a.[統一編號]
from cmoney.[dbo].[銀行分公司基本資料] a 
left join @基本資料 b on b.統一編號=a.統一編號
where a.年度=(select max(年度) from cmoney.[dbo].[銀行分公司基本資料])
and a.統一編號 is not null


declare @CB table(代號 nvarchar(50),名稱 nvarchar(50),轉換標的代號 nvarchar(50),債券擔保銀行代號 nvarchar(50))
insert @CB
select b.可轉債代碼,b.可轉債名稱,b.轉換標的,case when a.債券擔保銀行代號='000081' then '000018' else a.債券擔保銀行代號 end
from cb.dbo.CBData b
left join cb.[dbo].[CB擔保銀行代號]() a on b.可轉債代碼=a.代號
where b.TxDate=@t

--select *from 　@CB where 債券擔保銀行代號='000018'


declare @有效信評 table(授信戶ID nvarchar(50),信評等級 nvarchar(50))
insert @有效信評
select a.授信戶ID,a.信評等級
from [CB].[dbo].[有效信評] a
join (select 授信戶ID,max(資料日期) as 資料日期 from [CB].[dbo].[有效信評] where 信評等級<>'-'  group by 授信戶ID) b on b.授信戶ID=a.授信戶ID and b.資料日期=a.資料日期


declare @CB信評 table(代號 nvarchar(50),名稱 nvarchar(50),標的代號 nvarchar(50),信評_發行公司 nvarchar(50),信評_擔保銀行 nvarchar(50),信評 nvarchar(50),信用額度公司 nvarchar(50),[擔保比例] decimal(20,6))
insert @CB信評
select distinct a.代號,a.名稱,left(a.代號,4)
,isnull(cast(left(c1.信評等級,1) as int),9)
 as 信評_發行公司
,isnull(cast(left(c2.信評等級,1) as int),9)
 as 信評_擔保銀行
,case when isnull(cast(left(c1.信評等級,1) as int),9)<isnull(cast(left(c2.信評等級,1) as int),9) then isnull(cast(left(c1.信評等級,1) as int),9)
      else isnull(cast(left(c2.信評等級,1) as int),9) end
 as 信評
,case when isnull(cast(left(c2.信評等級,1) as int),9)<isnull(cast(left(c1.信評等級,1) as int),9) then b2.股票代號
      else left(a.代號,4) end
 as 信用額度公司
,isnull(g.[擔保比例],1) as [擔保比例]
from @CB a
left join @基本資料 b1 on b1.股票代號=left(a.代號,4)
left join @基本資料 b2 on b2.股票代號=a.債券擔保銀行代號
left join cb.[dbo].[可轉債擔保比例] g on g.[代號]=a.代號 and g.[債券擔保銀行代號]=a.債券擔保銀行代號 and @t between g.bdate and g.edate
left join @有效信評 c1 on c1.授信戶ID=b1.統一編號 
left join @有效信評 c2 on c2.授信戶ID=b2.統一編號
--order by a.代號


declare @CB信評group table(代號 nvarchar(50),名稱 nvarchar(50),信用額度公司 nvarchar(50),信評等級 nvarchar(50),[擔保比例] decimal(20,6))
insert @CB信評group
select  代號,名稱,信用額度公司
,case when cast(min(信評) as nvarchar(50)) in ('6','7','8','9') then '6+' 
	  else cast(min(信評) as nvarchar(50)) end
,sum([擔保比例])
from @CB信評
where 信評 is not null and 信用額度公司<>'000018'
group by 代號,名稱,信用額度公司


--select * from @CB信評group order by 代號


declare @信評等級限額temp table(信評 nvarchar(50),部位面額 decimal(20,4))
declare @信評等級限額 table(信評 nvarchar(50),部位面額 decimal(20,4))
/*
insert @信評等級限額
select case when d.信評等級 in('6','7') then '6+' else d.信評等級 end
,sum(a.股數*100*d.擔保比例)/1000000.
 as MV
from PL.[dbo].[債券他家部位] a
join cb.dbo.CBData b on b.TxDate=@t and b.可轉債代碼=a.代號
left join @CB信評group d on d.代號=a.代號
where a.TxDate=(select max(Txdate) from PL.[dbo].[債券他家部位] where a.Tag='CBAS')
group by case when d.信評等級 in('6','7') then '6+' else d.信評等級 end
*/
insert @信評等級限額temp 
select case when d.信評等級 in('6','7') then '6+' else d.信評等級 end
,sum(a.ASW_PRINCIPAL*d.擔保比例)/1000000.
 as MV
from dbmain.dbo.CBAS_ASW a
join cb.dbo.CBData b on b.TxDate=a.TxDate and b.可轉債代碼=a.ASW_CB
join cmoney.[dbo].[上市櫃公司基本資料] c on c.股票代號=b.轉換標的
left join @CB信評group d on d.代號=a.ASW_CB
where a.TxDate=@y --and a.ASW_POS_TYPE='PROP'
group by d.信評等級


insert @信評等級限額temp 
select case when d.信評等級 in('6','7') then '6+' else d.信評等級 end
,sum(a.股數*100*d.擔保比例)/1000000.
 as MV
from PL.[dbo].[債券他家部位] a
join cb.dbo.CBData b on b.TxDate=@t and b.可轉債代碼=a.代號
left join @CB信評group d on d.代號=a.代號
where a.TxDate=(select max(Txdate) from PL.[dbo].[債券他家部位] where a.Tag='CBAS')
group by case when d.信評等級 in('6','7') then '6+' else d.信評等級 end



insert @信評等級限額
select 信評,sum(部位面額) from @信評等級限額temp
group by 信評

-- 2017/5/15 William 加入，因為信評99代表的是無信評等級，所以用6+取代

--select * from @信評等級限額
--select * from @CB信評group 

declare @信評總和 dec(20,5) set @信評總和 = (select sum(部位面額)  from @信評等級限額)

declare @信評 table(
信評 nvarchar(20), 市值 dec(20,5), 信評等級限額_百萬 dec(20,5) , 限額百分比 dec(20,5), 總面額百分比 dec(20,5)
, 總面額上限比例 dec(20,5), 總面額上限啟動門檻_百萬 dec(20,5), 啟動 nvarchar(20), 超限 nvarchar(20) 
)
insert @信評 
select   b.內部信評或約當評等 as 信評,a.部位面額
,b.信評等級限額_百萬
,a.部位面額/b.信評等級限額_百萬
,a.部位面額/@信評總和
 as 百分比
 , b.總面額上限比例
 , b.總面額上限啟動門檻_百萬
 , case when @信評總和 > b.總面額上限啟動門檻_百萬 then '啟動' else '' end
 , case when b.信評等級限額_百萬 < a.部位面額 then '超限'
 when (@信評總和 > b.總面額上限啟動門檻_百萬) and (a.部位面額/@信評總和 >= b.總面額上限比例) then '超限'
 end 

from[DBMain].[dbo].交易資產交換CBAS_Credit端部位限額 b 
left join @信評等級限額 a on
b.內部信評或約當評等=(case when a.信評 in('6','7') then '6+' 
                           when a.信評='99' then '無'
						   else a.信評 end )
where b.ResetDate=@資料日期
order by  b.內部信評或約當評等

--select * from @信評

insert @信評
select  '總和', @信評總和, (select max(總面額上限_百萬) from [DBMain].[dbo].交易資產交換CBAS_Credit端部位限額 where ResetDate=@資料日期 ), null, null,null,null,null, null

select * from @信評
order by 信評


