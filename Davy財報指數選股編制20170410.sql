declare @t datetime set @t=(select max(���) from cmoney.dbo.�馬�L��Ʀ�)

create table #�]����(Q nvarchar(50),Y int,M nvarchar(50),D nvarchar(50))
insert #�]����
select '01',0,'5','15'
union
select '02',0,'8','31'
union
select '03',0,'11','15'
union
select '04',1,'3','31'

create table #���˰϶�(�~�u nvarchar(50),BDate datetime,EDate datetime)
insert #���˰϶�
select x.�~�u,x.���ˤ� as BDate,isnull(dbmain.dbo.tradingdateadd(-1,lead(x.���ˤ�) over(order by x.�~�u)),@t) as EDate
from (
	select cast(2000+ser as nvarchar(50))+b.Q as �~�u,dbmain.dbo.tradingdateadd(0,cast(cast(2000+ser as int)+b.Y as nvarchar(50))+'/'+b.M+'/'+b.D) as ���ˤ�
	from dbmain.dbo.tradingdate a
	cross join #�]���� b
	where ser between 1 and 16
) x

create table #����(�~�u nvarchar(50),�Ѳ��N�� nvarchar(50),�Ѳ��W�� nvarchar(50)
,�`�W�� int,�ꥻ���S�vrnk int,�վl�ާQ�vrnk int,�ꥻ���S�v decimal(20,10),�վl�ާQ�v decimal(20,10))
insert #����
select x.�~�u,x.�Ѳ��N��,x.�Ѳ��W��
,rank() over(partition by x.�~�u order by x.�ꥻ���S�vrnk+x.�վl�ާQ�vrnk,x.�ꥻ���S�v+x.�վl�ާQ�v desc)
 as �`�W��
,x.�ꥻ���S�vrnk
,x.�վl�ާQ�vrnk
,x.�ꥻ���S�v,x.�վl�ާQ�v
from (
	select x.�~�u,x.�Ѳ��N��,x.�Ѳ��W��
	,x.�ꥻ���S�v,x.�վl�ާQ�v
	,rank() over(partition by x.�~�u order by x.�ꥻ���S�v desc)
	 as �ꥻ���S�vrnk
	,rank() over(partition by x.�~�u order by x.�վl�ާQ�v desc)
	 as �վl�ާQ�vrnk
	from (
		select a.�~�u,a.�Ѳ��N��,a.�Ѳ��W��
		,isnull(d.[��~�Q�q(�d)],0)/(isnull(a.[�y�ʸ겣(�d)],0)-isnull(a.[�y�ʭt��(�d)],0)+isnull(a.[�D�y�ʸ겣(�d)],0)-isnull(a.[�L�θ겣(�d)],0))
		 as �ꥻ���S�v
		,isnull(d.[��~�Q�q(�d)],0)/(isnull(a.[���q�Ѫѥ�(�d)],0)/10.*c.�ѻ�+isnull(a.[�S�O�Ѫѥ�(�d)],0)+isnull(a.[�D�y�ʭt��(�d)],0)+isnull(a.[�@�~�Τ@��~�g������������t��(�d)],0))
		 as �վl�ާQ�v
		from cmoney.[dbo].[�uIFRS�]��(�겣�t��)] a
		join (
			select cast(datepart(yy,tradingdate) as nvarchar(50))+'0'+cast(datepart(qq,tradingdate) as nvarchar(50)) as �~�u
			,max(tradingdate) as TDate
			from dbmain.dbo.tradingdate
			group by datepart(yy,tradingdate),datepart(qq,tradingdate)
		) b on b.�~�u=a.�~�u
		join testdb.[dbo].[�j�L�����ѨC�饫��] c on c.�~���=b.TDate and c.������=a.�Ѳ��N��
		--join testdb.[dbo].[�d�R�����ѨC�饫��] c on c.�~���=b.TDate and c.������=a.�Ѳ��N��
		left join cmoney.[dbo].[�uIFRS�]��(�l�q��u)] d on d.�~�u=a.�~�u and d.�Ѳ��N��=a.�Ѳ��N��
	) x
) x

select x.���
,isnull(log( (x.IX/y.���L��) / lag(x.IX/y.���L��) over(order by x.���) )
       ,x.ret-log(y.���L��/(y.���L��-y.���^))
	   )
 as RSret
,x.IX as MF30,y.���L�� as ���S����
from (
	select x.�~�u,x.���
	,100*exp(sum(x.ret) over(order by x.���))
	 as IX
	,x.Ret,x.N
	from (
		select a.�~�u,c.���
		,avg(case when c.���L��-c.���^<>0 then log(c.���L��/(c.���L��-c.���^)) end)
		 as Ret
		,sum(case when c.�Ѳ��N�� is not null then 1 else 0 end)
		 as N
		from #���� a
		join #���˰϶� b on b.�~�u=a.�~�u
		join cmoney.dbo.�馬�L��Ʀ� c on c.��� between b.BDate and b.EDate and c.�Ѳ��N��=a.�Ѳ��N��
		where a.�`�W��<=30-- and c.���L��-c.���^=0
		group by a.�~�u,c.���
	) x
) x
left join cmoney.dbo.�馬�L��Ʀ� y on y.���=x.��� and y.�Ѳ��N��='TWA02'
order by x.�~�u,x.���

select a.�~�u,a.�Ѳ��N��,a.�Ѳ��W��,a.�`�W��,a.�ꥻ���S�vrnk,a.�վl�ާQ�vrnk,a.�ꥻ���S�v,a.�վl�ާQ�v
,b.BDate,b.EDate
from #���� a
join #���˰϶� b on b.�~�u=a.�~�u
where a.�`�W��<=30 and a.�~�u>='200704'


drop table #���˰϶�
drop table #�]����
drop table #����


