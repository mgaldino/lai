
-- importando caminhos dos pedidos para inserir na tabela

use tblai;

-- DROP TEMPORARY TABLE IF EXISTS tmp_import;


-- cria tabela vazia para receber os dados do csv
CREATE TEMPORARY TABLE tmp_import (
Codigo INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY,
Protocolo VARCHAR (200) NULL,
CodigoPedidoInteracao INTEGER NULL,
ArquivoFullPath Varchar(150) NULL,
Ativo integer NULL,
Criacao Varchar(250) NULL,
Alteracao Varchar(500) NULL,
CodigoStatusExportacaoES Varchar(500) NULL,
Arquivo VARCHAR(500) NULL
);

-- importa o conteúdo do csv na minha tabela local que eu criei acima (tmp_import)
-- output vai ter message mde quantas linhas foram inseridas.
LOAD DATA LOCAL INFILE 'C:/Users/mgaldino/2017/Ford/AchadosePedidos/Arquivos/Anexos/CGM/tabela_anexos_cgm_parte1.csv' INTO TABLE tmp_import
CHARACTER SET UTF8
FIELDS TERMINATED BY ';' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES
;


-- teste das 20primeiras linhas
select * from tmp_import 
limit 20;

-- demora 8 min
-- DROP TEMPORARY TABLE IF EXISTS import_table;

CREATE TEMPORARY TABLE IF NOT EXISTS import_table
 (SELECT NULL as Codigo, 
		i.Codigo as CodigoPedidoInteracao,
        ti.ArquivoFullPath,
        ti.Ativo,
        NOW() as Criacao,
        NOW() as Alteracao,
        ti.CodigoStatusExportacaoES,
        ti.Arquivo
FROM tblai.pedidos p
inner join tblai.usuarios u
on p.CodigoUsuario = u.Codigo
inner join tblai.pedidos_interacoes i
on i.CodigoPedido = p.Codigo
inner join tmp_import ti
on ti.Protocolo = p.Protocolo
where u.Codigo = 11
and CodigoTipoPedidoResposta = 1)
;

-- olhar nooutput se o número de linhas está certo

-- teste
select * from import_table
limit 30
;



-- insert na tabela pedidos_anexos.

INSERT INTO pedidos_anexos (Codigo, CodigoPedidoInteracao, ArquivoFullPath, Ativo, Criacao, 
Alteracao, CodigoStatusExportacaoES,
Arquivo) 
		select *
        from import_table 
; -- 471