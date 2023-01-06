select numero_filhos, grau_instrucao, round(avg(idade)) as media_idade
from cap16."TB_FUNC"
where reg_procedencia = 'capital' 
	and estado_civil = 'casado'
	and salario_hora > (select avg(salario_hora) from cap16."TB_FUNC")
group by numero_filhos, grau_instrucao
order by avg(idade) desc
