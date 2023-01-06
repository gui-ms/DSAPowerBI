select avg(f.salario_hora), e.estado
from cap16."TB_FUNC" f, cap16."TB_ENDERECO" e
where f.id = e.id_func
group by e.estado