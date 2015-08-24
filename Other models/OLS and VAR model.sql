select date_format(first_successful_payment, '%x-%v') as week
,date_format(min(first_successful_payment), '%Y-%m-%d') as date
,count(first_successful_payment) as WNU_GBPINR
from report_user_characteristics
where first_ccy_pair = 'GBP > INR'
group by 1;

select date_format(first_successful_payment, '%x-%v') as week
,date_format(min(first_successful_payment), '%Y-%m-%d') as date
,count(first_successful_payment) as WNU
from report_user_characteristics
group by 1