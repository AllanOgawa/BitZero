Algoritmo "BITZERO"
//          ALAN MINORU OTANI RA: 21163119-2
//                ALLAN OGAWA RA: 21105670-2
//           HEITOR BRANDOLIN RA: 21188666-2
//    MICHELLE FREGNANI OTANI RA: 21163120-2
//  RODRIGO BRANCO NASCIMENTO RA: 1908804-2

VAR
     CLIENTE: VETOR[1..150,1..6] de CARACTER
     PARCELA: VETOR[1..80,1..7]  de CARACTER
   NomeParce: VETOR[1..80]       de CARACTER

        carteiracod: VETOR[1..6] de CARACTER
        carteiranom: VETOR[1..6] de CARACTER

                              x, y: INTEIRO
       totalpar, totalcli, diasqnt: INTEIRO
   linha, aguarda, conf, dataatual: CARACTER
              importcli, importpar: LOGICO

//=============================================================================================================================
//========== LEITURA DE DADOS =================================================================================================
//=============================================================================================================================

Funcao LeCampo(VAR p:INTEIRO):CARACTER
VAR
  a,caract:CARACTER
INICIO
  a<-""
  caract<-COPIA(linha,p,1)
  Enquanto ASC(caract)<>34 Faca
    a<-a+caract
    p<-p+1
    caract<-COPIA(linha,p,1)
  FimEnquanto
  p<-p+3
  Retorne a
FimFuncao

//=============================================================================================================================

Procedimento LeCliente //Leitura do Arquivo CLIENTES
Arquivo "CLIENTES.txt"
VAR
    a:INTEIRO
  tmp:CARACTER
INICIO
  Para x de 1 Ate 147 Faca
    Leia(linha)
    a<-2
    CLIENTE[x,1]<- LeCampo(a)//CODIGO
    CLIENTE[x,2]<- LeCampo(a)//NOME
    CLIENTE[x,3]<- LeCampo(a)//CIDADE
    CLIENTE[x,4]<- LeCampo(a)//UF
    tmp<- LeCampo(a)
    tmp<- LeCampo(a)
    tmp<- LeCampo(a)
    CLIENTE[x,5]<- LeCampo(a)//CPF
    CLIENTE[x,6]<-"="//STATUS DO CLIENTE
    totalcli<-totalcli+1
  FimPara
  importcli<-VERDADEIRO
FimProcedimento

//=============================================================================================================================

Procedimento LeParcela //Leitura do Arquivo PARCELA
Arquivo "PARCELAS.txt"
VAR
  a:INTEIRO
INICIO
  Para x de 1 Ate 76 Faca
    Leia(linha)
    a<-2
    PARCELA[x,1]<- LeCampo(a)//PARCELA
    PARCELA[x,2]<- LeCampo(a)//CODIGO
    PARCELA[x,3]<- LeCampo(a)//DATA VENCIMENTO
    PARCELA[x,4]<- LeCampo(a)//DATA RECEBIMENTO
    PARCELA[x,5]<- LeCampo(a)//VALOR
    PARCELA[x,6]<- LeCampo(a)//BANCO
    PARCELA[x,7]<-"="//STATUS DA PARCELA
    totalpar<-totalpar+1
  FimPara
  importpar<-VERDADEIRO
FimProcedimento

//=============================================================================================================================

Procedimento CarregaBancos
INICIO
  carteiracod[1]<-"0"
  carteiracod[2]<-"001"
  carteiracod[3]<-"104"
  carteiracod[4]<-"237"
  carteiracod[5]<-"260"
  carteiracod[6]<-"341"

  carteiranom[1]<-"Carteira"
  carteiranom[2]<-"Banco do Brasil"
  carteiranom[3]<-"Caixa Econômica Federal"
  carteiranom[4]<-"Bradesco"
  carteiranom[5]<-"Nubank"
  carteiranom[6]<-"Itau"
FimProcedimento

//=============================================================================================================================
//========== CLASSIFICAÇÃO ====================================================================================================
//=============================================================================================================================

Procedimento class_parcela(type: CARACTER) //Classificação de Parcela (PARCELAS)
VAR
  a,b,data1,data2:INTEIRO
              tmp:CARACTER
INICIO
  Se (importpar=FALSO) Entao
     msg("ATENÇÃO: Tabela de Parcelas está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    EscrevaL
    Escreva("Classificando ")
    Para a de 1 Ate totalpar-1 Faca
      Se a MOD 3=0 Entao
        Escreva(">")
      FimSe
      Para b de a+1 Ate totalpar Faca
        data1<-CaracPNum(COPIA(PARCELA[a,3],7,2)+COPIA(PARCELA[a,3],4,2)+COPIA(PARCELA[a,3],1,2))
        data2<-CaracPNum(COPIA(PARCELA[b,3],7,2)+COPIA(PARCELA[b,3],4,2)+COPIA(PARCELA[b,3],1,2))
        Se ((type="1") e (PARCELA[a,1]>PARCELA[b,1])) ou ((type="2") e (data1>data2)) ou ((type="3") e (NomeParce[a]>NomeParce[b]))Entao
          baSe_parc(a,b)
        FimSe
        Se type="3" Entao
          Se (NomeParce[a]=NomeParce[b]) Entao
            Se (data1>data2) Entao
              tmp<-PARCELA[a,3]
              PARCELA[a,3]<-PARCELA[b,3]
              PARCELA[b,3]<-tmp
            FimSe
          FimSe
        FimSe
      FimPara
    FimPara
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento class_cliente(typo:CARACTER)
VAR
  a,b:INTEIRO
INICIO
  Se (importcli=FALSO) Entao
    msg("ATENÇÃO: Tabela de Clientes está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    EscrevaL
    Escreva("Classificando ")
    Para a de 1 Ate totalpar-1 Faca
      Se a MOD 3=0 Entao
        Escreva(">")
      FimSe
      Para b de a+1 Ate totalcli Faca
        Se ((typo="1") e (CLIENTE[a,1]>CLIENTE[b,1])) ou ((typo="2") e (CLIENTE[a,2]>CLIENTE[b,2])) Entao
          baSe_cliente(a,b)
        FimSe
      FimPara
    FimPara
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento base_parc(a,b:INTEIRO)//A baSe da parte de classificação do arquivo (PARCELAS)
VAR
  tmp:CARACTER
INICIO
  tmp<-PARCELA[a,1]
  PARCELA[a,1]<-PARCELA[b,1]
  PARCELA[b,1]<-tmp

  tmp<-PARCELA[a,2]
  PARCELA[a,2]<-PARCELA[b,2]
  PARCELA[b,2]<-tmp

  tmp<-PARCELA[a,3]
  PARCELA[a,3]<-PARCELA[b,3]
  PARCELA[b,3]<-tmp

  tmp<-PARCELA[a,4]
  PARCELA[a,4]<-PARCELA[b,4]
  PARCELA[b,4]<-tmp

  tmp<-PARCELA[a,5]
  PARCELA[a,5]<-PARCELA[b,5]
  PARCELA[b,5]<-tmp

  tmp<-PARCELA[a,6]
  PARCELA[a,6]<-PARCELA[b,6]
  PARCELA[b,6]<-tmp

  tmp<-PARCELA[a,7]
  PARCELA[a,7]<-PARCELA[b,7]
  PARCELA[b,7]<-tmp

  tmp<-NomeParce[a]
  NomeParce[a]<-NomeParce[b]
  NomeParce[b]<-tmp
FimProcedimento

//=============================================================================================================================

Procedimento base_cliente(a,b:INTEIRO)//A baSe da parte de classificação do arquivo (CLIENTES)
VAR
  tmp:CARACTER
INICIO
  tmp<-CLIENTE[a,1]
  CLIENTE[a,1]<-CLIENTE[b,1]
  CLIENTE[b,1]<-tmp

  tmp<-CLIENTE[a,2]
  CLIENTE[a,2]<-CLIENTE[b,2]
  CLIENTE[b,2]<-tmp

  tmp<-CLIENTE[a,3]
  CLIENTE[a,3]<-CLIENTE[b,3]
  CLIENTE[b,3]<-tmp

  tmp<-CLIENTE[a,4]
  CLIENTE[a,4]<-CLIENTE[b,4]
  CLIENTE[b,4]<-tmp

  tmp<-CLIENTE[a,5]
  CLIENTE[a,5]<-CLIENTE[b,5]
  CLIENTE[b,5]<-tmp

  tmp<-CLIENTE[a,6]
  CLIENTE[a,6]<-CLIENTE[b,6]
  CLIENTE[b,6]<-tmp
FimProcedimento

//=============================================================================================================================
//========== VERIFICAÇÃO ======================================================================================================
//=============================================================================================================================

Funcao verificar_cliente(cl:CARACTER):INTEIRO
VAR
  a:INTEIRO
INICIO
  a<-0
  Repita
    a<-a+1
  Ate (a>=totalcli) ou (cl=CLIENTE[a,1]) e (CLIENTE[A,6]="=")
  Se (cl=CLIENTE[a,1]) e (CLIENTE[A,6]="=") Entao
    Retorne a
  SeNao
    Retorne 0
  FimSe
FimFuncao

//=============================================================================================================================

Funcao verificar_parcela(pa:CARACTER):INTEIRO
VAR
  a:INTEIRO
INICIO
  a<-0
  Repita
    a<-a+1
  Ate (a>=totalpar) ou (pa=PARCELA[a,1]) e (PARCELA[a,7]="=")
  Se (pa=parcela[a,1]) e (PARCELA[a,7]="=") Entao
    Retorne a
  SeNao
    Retorne 0
  FimSe
FimFuncao

//=============================================================================================================================

Funcao verificar_banco(ba:CARACTER):INTEIRO
VAR
  a:INTEIRO
INICIO
  a<-0
  Repita
    a<-a+1
  Ate (a=6) ou (ba=carteiraCod[a])
  Se (ba=carteiraCod[a]) Entao
    Retorne a
  SeNao
    Retorne 0
  FimSe
FimFuncao

//=============================================================================================================================

Funcao verificar_data(data:CARACTER):LOGICO
VAR
        dia,mes,ano:INTEIRO
  diava,mesva,anova:LOGICO
INICIO
  Se (Compr(data)=8) Entao
    diava<-FALSO
    mesva<-FALSO
    anova<-FALSO
    dia<-CaracPNum(COPIA(data,1,2))
    mes<-CaracPNum(COPIA(data,4,2))
    ano<-CaracPNum(COPIA(data,7,2))
    Se (ano>=0) e(ano<=99) Entao
      anova<-VERDADEIRO
      Se (mes>=1) e (mes<=12) Entao
        mesva<-VERDADEIRO
        Se (mes=2) e (ano MOD 4=0) e (dia>=1) e (dia<=29) Entao
          diava<-VERDADEIRO
        FimSe
        Se (mes=2) e (ano MOD 4<>0) e (dia>=1) e (dia<=28) Entao
          diava<-VERDADEIRO
        FimSe
      FimSe
      Se (dia>=1) e (dia<=31) e ((mes=1) ou (mes=3) ou (mes=5) ou (mes=7) ou (mes=8) ou (mes=10) ou (mes=12)) Entao
        diava<-VERDADEIRO
      FimSe
      Se (dia>=1) e (dia<=30) e ((mes=4) ou (mes=6) ou (mes=9) ou (mes=11)) Entao
        diava<-VERDADEIRO
      FimSe
    FimSe
  SeNao
    Retorne FALSO
  FimSe
  Se (diava=VERDADEIRO) e (mesva=VERDADEIRO) e (anova=VERDADEIRO) Entao
    Retorne VERDADEIRO
  SeNao
    Retorne FALSO
  FimSe
FimFuncao

//=============================================================================================================================

Funcao verificar_cpf(cpf:CARACTER) : LOGICO
VAR
   num1,num2,num3,num4,num5,num6,num7,num8,num9,num10,num11,soma1,soma2:INTEIRO
                                                          resto1,resto2:REAL
INICIO
  Se compr(cpf)<>14 Entao
    Retorne FALSO
  SeNao
    num1 <-CaracPNum(COPIA(cpf,1,1))
    num2 <-CaracPNum(COPIA(cpf,2,1))
    num3 <-CaracPNum(COPIA(cpf,3,1))
    num4 <-CaracPNum(COPIA(cpf,5,1))
    num5 <-CaracPNum(COPIA(cpf,6,1))
    num6 <-CaracPNum(COPIA(cpf,7,1))
    num7 <-CaracPNum(COPIA(cpf,9,1))
    num8 <-CaracPNum(COPIA(cpf,10,1))
    num9 <-CaracPNum(COPIA(cpf,11,1))
    num10<-CaracPNum(COPIA(cpf,13,1))
    num11<-CaracPNum(COPIA(cpf,14,1))
    Se (num1=num2) E (num2=num3) E (num3=num4) E (num4=num5) E (num5=num6) E (num6=num7) E (num7=num8) E (num8=num9) E (num9=num10) E (num10=num11) Entao
      Retorne FALSO
    SeNao
      soma1 <-num1*10 + num2*9 + num3*8 + num4*7 + num5*6 + num6*5 + num7*4 + num8*3 + num9*2
      resto1<-(soma1*10) MOD 11

      Se resto1=10 Entao
         resto1<-0
      FimSe
      soma2 <-num1*11 + num2*10 + num3*9 + num4*8 + num5*7 + num6*6 + num7*5 + num8*4 + num9*3 + num10*2
      resto2<-(soma2*10) MOD 11

      Se resto2=10 Entao
         resto2<-0
      FimSe

      Se (resto1 = num10) E (resto2 = num11) Entao
         Retorne VERDADEIRO
      SeNao
         Retorne FALSO
      FimSe
    FimSe
  FimSe
FimFuncao

//=============================================================================================================================
//========== INCLUSÃO =========================================================================================================
//=============================================================================================================================

Procedimento InclusaoCliente
VAR
  Cod_cli:CARACTER
  Ind_cli:INTEIRO
    o,cpf:CARACTER
INICIO
  Se (importcli=FALSO) Entao
    msg("ATENÇÃO: Tabela de Clientes está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    Repita
      bitzero("CADASTRO DE CLIENTE")
      Escreva("Digite o código do cliente: ")
      Leia(Cod_cli)
      Se (Cod_cli="") Entao
        msg("Código não aceitável. Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
      FimSe
      Ind_cli<-Verificar_cliente(Cod_cli)
      Se (Ind_cli<>0) Entao
        Mostra_cliente(Ind_cli)
        msg("Cliente já Cadastrado. Pressione >>ENTER<< Para continuar..",VERDADEIRO,FALSO)
      FimSe
    Ate (Ind_cli=0) e (Cod_cli<>"")
    totalcli<-totalcli+1
    CLIENTE[totalcli,1]<-Cod_cli
    Escreva ("Nome: ")
    Leia(CLIENTE[totalcli,2])
    Escreva("Cidade: ")
    Leia(CLIENTE[totalcli,3])
    Escreva("Estado (UF): ")
    Leia(CLIENTE[totalcli,4])

    Repita
      Escreva("Incluir (1)-CPF ou (2)-CNPJ: ")
      Leia(o)
    Ate (o="1") ou (o="2")
    Se (o="1") Entao
      Escreva("CPF(com pontos e hifen): ")
      Leia(cpf)
      Repita
        Se verificar_cpf(cpf) = FALSO Entao
          Escreva("O CPF é inválido! Digite novamente: ")
          Leia(cpf)
        FimSe
      Ate verificar_cpf(cpf) = VERDADEIRO
      EscrevaL("O CPF é válido!")
      CLIENTE[totalcli,5]<-cpf
    SeNao
      Escreva("CNPJ: ")
      Leia(cpf)
      CLIENTE[totalcli,5]<-cpf
    FimSe
    EscrevaL
    conf<-Confirma("Confirma inclusão? (S/N) ")
    Se (conf="S") ou (conf="s") Entao
      CLIENTE[TotalCli,6]<-"="
      EscrevaL
      EscrevaL("INCLUSÃO REALIZADA COM SUCESSO!")
      msg("Pressione >>ENTER<< Para continuar!",VERDADEIRO,VERDADEIRO)
    SeNao
      EscrevaL
      EscrevaL("INCLUSÃO NEGADA")
      msg("Pressione >>ENTER<< Para voltar ao menu",VERDADEIRO,VERDADEIRO)
      totalcli<-totalcli-1
    FimSe
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento InclusaoParcela
VAR
   pa,cli,ban,val,ven,rec:CARACTER
  apa,acli,aban,aven,arec:INTEIRO
                        a:LOGICO
INICIO
  Se (importpar=FALSO) Entao
    msg("ATENÇÃO: Tabela de Parcelas está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    Repita
      bitzero("INCLUIR PARCELA")
      Escreva("Código da parcela: ")
      Leia(pa)
      apa<-verificar_parcela(pa)
      Se (apa<>0) Entao
        msg("Parcela já existente. Pressione >>ENTER<< Para digitar outra parcela",VERDADEIRO,VERDADEIRO)
      FimSe
      Se (pa="") Entao
        msg("Parcela não aceitável. Pressione >>ENTER<< Para digitar outra parcela",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (apa=0) e (pa<>"")
    totalpar<-totalpar+1

    Repita
      bitzero("INCLUIR PARCELA")
      EscrevaL("Código da parcela: ",pa)
      Escreva("Código do cliente: ")
      Leia(cli)
      acli<-verificar_cliente(cli)
      Se (acli=0) ou (cli="") Entao
        msg("Cliente Inexistente. Pressione >>ENTER<< Para digitar outro cliente",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (acli<>0) e (cli<>"")

    Repita
      Mostra_Cliente_Banco(pa,cli,acli)
      Escreva("            Banco: ")
      Leia(ban)
      aban<-verificar_banco(ban)
      Se (aban<>0) Entao
        Mostra_Cliente_Banco(pa,cli,acli)
        EscrevaL("            Banco: ",ban:4,"(",carteiranom[aban],")")
        Escreva("            Valor: ")
        Leia(val)
      FimSe
      Se (aban=0) Entao
        msg("Banco Inexistente. Pressione >>ENTER<< Para digitar outro banco",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (aban<>0)

    Repita
      Mostra_Cliente_Banco(pa,cli,acli)
      EscrevaL("            Banco: ",ban:4,"(",carteiranom[aban],")")
      EscrevaL("            Valor: ",val)
      Escreva("  Data Vencimento: ")
      Leia(ven)
      a<-verificar_data(ven)
      Se (a=FALSO) ou (Compr(ven)<>8) Entao
        msg("Data inválida. Pressione >>ENTER<< Para digitar outra data (DD/MM/AA)",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (a=VERDADEIRO)

    Repita
      Mostra_Cliente_Banco(pa,cli,acli)
      EscrevaL("            Banco: ",ban:4,"(",carteiranom[aban],")")
      EscrevaL("            Valor: ",val)
      EscrevaL("  Data Vencimento: ",ven)
      Escreva(" Data Recebimento: ")
      Leia(rec)
      Se (rec<>"") Entao
        a<-verificar_data(rec)
        Se (a=FALSO) ou (Compr(rec)<>8) Entao
          msg("Data inválida. Pressione >>ENTER<< Para digitar outra data (DD/MM/AA)",VERDADEIRO,VERDADEIRO)
        FimSe
      FimSe
    Ate (a=VERDADEIRO) ou (rec="")
    EscrevaL
    conf<-Confirma("Confirma inclusão? (S/N) ")
    Se (conf="S") ou (conf="s") Entao
      PARCELA[TotalPar,7]<-"="
      PARCELA[TotalPar,1]<-pa
      PARCELA[TotalPar,2]<-cli
      PARCELA[totalpar,3]<-ven
      PARCELA[totalpar,4]<-rec
      PARCELA[totalpar,5]<-val
      PARCELA[totalpar,6]<-ban
      EscrevaL
      EscrevaL("INCLUSÃO REALIZADA COM SUCESSO!")
      msg("Pressione >>ENTER<< Para continuar!",VERDADEIRO,VERDADEIRO)
    SeNao
      EscrevaL
      EscrevaL("INCLUSÃO NEGADA")
      msg("Pressione >>ENTER<< Para voltar ao menu",VERDADEIRO,VERDADEIRO)
      totalpar<-totalpar-1
    FimSe
  FimSe
FimProcedimento

//=============================================================================================================================
//========== MANUTENÇÃO =======================================================================================================
//=============================================================================================================================

Procedimento ManutencaoCliente
VAR
  tmp,y2,cli,o:CARACTER
        p,acli:INTEIRO
INICIO
  Se (importcli=FALSO) ou (importpar=FALSO) Entao
    msg("ATENÇÃO: Tabela de Parcelas e/ou Clientes está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    Repita
      bitzero("manutenção de cliente")
      Escreva("Digite o código do cliente: ")
      Leia(cli)
      acli<-verificar_cliente(cli)
      Se (acli=0) ou (cli="") Entao
        EscrevaL("CLIENTE INEXISTENTE ou CÓDIGO INCORRETO!")
        msg("Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (acli<>0) e (cli<>"")
    Mostra_cliente(acli)
    EscrevaL

    Repita
      bitzero("manutenção de cliente")
      Escreva("Digite o código do cliente: ",cli)
      EscrevaL
      Mostra_cliente(acli)
      EscrevaL
      EscrevaL("1- Alterar nome")
      EscrevaL("2- Alterar cidade")
      EscrevaL("3- Alterar estado")
      EscrevaL("4- Alterar CPF/CNPJ")
      EscrevaL("5- Excluir")
      EscrevaL("9- voltar")
      EscrevaL
      Escreva("Opção: ")
      Leia(y2)
      EscrevaL
      Escolha y2
        Caso "1"
          Escreva("Digite o novo nome: ")
          Leia(tmp)
          EscrevaL
          conf<-Confirma("Confirma alteração (S/N)?")
          Se (conf="S") ou (conf="s") Entao
            CLIENTE[acli,2]<-tmp
            EscrevaL
            EscrevaL("Nome alterado com sucesso!")
            msg("Pressione >>ENTER<< Para continuar..",verdadeiro,verdadeiro)
          FimSe
        Caso "2"
          Escreva("Digite a nova cidade: ")
          Leia(tmp)
          EscrevaL
          conf<-Confirma("Confirma alteração (S/N)?")
          Se (conf="S") ou (conf="s") Entao
            CLIENTE[acli,3]<-tmp
            EscrevaL
            EscrevaL("Cidade alterada com sucesso!")
            msg("Pressione >>ENTER<< Para continuar..",verdadeiro,verdadeiro)
          FimSe
       Caso "3"
          Escreva("Digite o novo estado: ")
          Leia(tmp)
          EscrevaL
          conf<-Confirma("Confirma alteração (S/N)?")
          Se (conf="S") ou (conf="s") Entao
            CLIENTE[acli,4]<-tmp
            EscrevaL
            EscrevaL("Estado alterado com sucesso!")
            msg("Pressione >>ENTER<< Para continuar..",verdadeiro,verdadeiro)
          FimSe
        Caso "4"
          Repita
            Escreva("Digite o novo (1)-CPF ou (2)-CNPJ: ")
            Leia(o)
          Ate (o="1") ou (o="2")
          Se (o="1") entao
            Escreva("CPF(com pontos e hifen): ")
            leia(tmp)
            Repita
              Se validaCPF(tmp) = FALSO Entao
                Escreva("O CPF é inválido! Digite novamente: ")
                Leia(tmp)
              FimSe
            Ate validaCPF(tmp) = VERDADEIRO
            EscrevaL("O CPF é válido!")
            CLIENTE[totalcli,5]<-tmp
          SeNao
            Escreva("CNPJ: ")
            Leia(tmp)
            CLIENTE[totalcli,5]<-tmp
          fimse
          EscrevaL
          conf<-Confirma("Confirma alteração (S/N)?")
          Se (conf="S") ou (conf="s") Entao
            CLIENTE[acli,5]<-tmp
            EscrevaL
            EscrevaL("CPF/CPJ alterado com sucesso!")
            msg("Pressione >>ENTER<< Para continuar..",verdadeiro,verdadeiro)
          FimSe
        Caso "5"
          p<-0
          Para y de 1 Ate totalpar Faca
            Se (PARCELA[y,2]=CLIENTE[acli,1]) e (CLIENTE[y,6]="=") Entao
              p<-p+1
            FimSe
          FimPara
          Se (p>0) Entao
            EscrevaL("EXCLUSÃO NAO PERMITIDA!")
            EscrevaL("Cliente possui parcelas.")
            msg("Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
          SeNao
            conf<-Confirma("Confirma exclusão? (S/N)")
            Se (conf="S") ou (conf="s") Entao
              CLIENTE[acli,6]<-""
              EscrevaL
              EscrevaL("CLIENTE EXCLUIDO COM SUCESSO!")
              msg("Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
            FimSe
          FimSe
      FimEscolha
    Ate (y2>="1") e (y2<="9")
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento ManutencaoParcela
VAR
                tmp,y2,par:CARACTER
  aban,acli,apar,aVen,aRec:INTEIRO
INICIO
  Se (importcli=FALSO) ou (importpar=FALSO) Entao
    msg("ATENÇÃO: Tabela de Parcelas e/ou Clientes está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    Repita
      bitzero("Manutenção de Parcela")
      Escreva ("Código da Parcela: ")
      Leia(par)
      apar<-verificar_parcela(par)
      Se (apar=0) ou (par="") Entao
        msg("Parcela Inexistente. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (apar<>0) e (par<>"")

    Repita
      bitzero("Manutenção de Parcela")
      EscrevaL ("Código da Parcela: ",par)
      Mostra_Parcela(apar)
      EscrevaL
      EscrevaL("1 - Alterar Cliente")
      EscrevaL("2 - Alterar Banco")
      EscrevaL("3 - Alterar Valor")
      EscrevaL("4 - Alterar Data Vencimento")
      EscrevaL("5 - Lançar Recebimento")
      EscrevaL("6 - Excluir")
      EscrevaL("9 - Retornar")
      EscrevaL
      Escreva("Opção: ")
      Leia(y2)
      EscrevaL
      Escolha y2
        Caso "1"
          Repita
            Mostra_Manutencao_Parcela(par,y2,apar,1)
            Escreva("Informe o novo Cliente: ")
            Leia(tmp)
            acli<-verificar_cliente(tmp)
            Se (acli=0) ou (tmp="") Entao
              msg("Cliente inexistente. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
            FimSe
          Ate (acli<>0) e (tmp<>"")
          EscrevaL(CLIENTE[acli,2])
          Mostra_editado_sucesso(tmp,apar,2)
        Caso "2"
          Repita
            Mostra_Manutencao_Parcela(par,y2,apar,2)
            Escreva ("Informe o novo Banco: ")
            Leia(tmp)
            aBan<-verificar_banco(tmp)
            Se (aBan=0) ou (tmp="") Entao
              msg("Banco inexistente. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
            FimSe
          Ate (aBan<>0) e (tmp<>"")
          EscrevaL(carteiranom[aBan])
          Mostra_editado_sucesso(tmp,apar,6)
        Caso "3"
          Mostra_Manutencao_Parcela(par,y2,apar,1)
          Escreva ("Informe o novo Valor: ")
          Leia(tmp)
          Mostra_editado_sucesso(tmp,apar,5)
        Caso "4"
          Repita
            Mostra_Manutencao_Parcela(par,y2,apar,1)
            Escreva("Informe a Data de Vencimento: ")
            Leia(tmp)
            aVen<-verificar_data(tmp)
            Se (aVen=0) ou (tmp="") Entao
              msg("Data não aceitável. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
            FimSe
          Ate (aVen<>0) e (tmp<>"")
          Mostra_editado_sucesso(tmp,apar,3)
        Caso "5"
          Repita
            Mostra_Manutencao_Parcela(par,y2,apar,1)
            Escreva("Informe a Data de Recebimento: ")
            Leia(tmp)
            aRec<-verificar_data(tmp)
            Se (aRec=0) e (tmp<>"") Entao
              msg("Data não aceitável. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
            FimSe
          Ate (aRec<>0) ou (tmp="")
          Mostra_editado_sucesso(tmp,apar,4)
        Caso "6"
          conf<-confirma("Confirma Exclusão (S/N)? ")
          Se (conf="S") ou (conf="s") Entao
            PARCELA[apar,7]<-""
            EscrevaL
            EscrevaL("PARCELA EXCLUIDA COM SUCESSO!")
            msg("Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
          SeNao
            EscrevaL
            EscrevaL("PARCELA NÃO EXCLUIDA")
            msg("Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
          FimSe
      FimEscolha
    Ate (y2>="1") e (y2<="9")
  FimSe
FimProcedimento

//=============================================================================================================================
//========== CONSULTA =========================================================================================================
//=============================================================================================================================

Procedimento ConsultaCliente
VAR
  cidade,uf,ordem:CARACTER
              lin:INTEIRO
INICIO
  Se (importcli=FALSO) Entao
    msg("ATENÇÃO: Tabela de Clientes está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    lin<-0
    bitzero("Listagem de Clientes")
    Escreva(" Qual Cidade? >>ENTER<< Para todas: ")
    Leia(cidade)

    bitzero("Listagem de Clientes")
    EscrevaL(" Qual Cidade? >>ENTER<< Para todas: ",cidade)
    Escreva(" Qual Estado? >>ENTER<< Para todos: ")
    Leia(uf)

    Repita
      bitzero("Listagem de Clientes")
      EscrevaL(" Qual Cidade? >>ENTER<< Para todas: ",cidade)
      EscrevaL(" Qual Estado? >>ENTER<< Para todos: ",uf)
      Escreva("      Ordem por (1-Código  2-Nome): ")
      Leia(ordem)
      Se (ordem<>"1") e (ordem<>"2") e (ordem<>"") Entao
        msg("Ordem Inválida. Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (ordem="1") ou (ordem="2") ou (ordem="")
    Se (ordem="") Entao
      ordem<-"2"
    FimSe
    class_cliente(ordem)
    LimpaTela
    Para x de 1 Ate totalcli faca
      Se ((cidade=CLIENTE[x,3]) ou (cidade="")) e ((uf=CLIENTE[x,4]) ou (uf="")) e (CLIENTE[x,6]="=") Entao
        Se lin=0 Entao
          EscrevaL("    COD   NOME                     CIDADE               UF  CPF/CPNJ")
          EscrevaL("-------------------------------------------------------------------------------")
        FimSe
        EscrevaL(x:3," ",CLIENTE[x,1]:5," ",CLIENTE[x,2]:25,CLIENTE[x,3]:20," ",CLIENTE[x,4]:2,"  ",CLIENTE[x,5])
        lin<-lin+1
        Se lin=21 Entao
          EscrevaL("-------------------------------------------------------------------------------")
          msg("< Pressione >>ENTER<< Para continuar >",VERDADEIRO,VERDADEIRO)
          lin<-0
        FimSe
      FimSe
    FimPara
    Se lin>0 Entao
      EscrevaL("-------------------------------------------------------------------------------")
      msg("< Pressione >>ENTER<< Para retornar ao Menu > ",VERDADEIRO,VERDADEIRO)
    FimSe
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento ConsultaParcela
VAR
      lin,acli,aban:INTEIRO
  cli,ban,par,ord,a:CARACTER
INICIO
  lin<-0
  Se (importpar=FALSO) ou (importcli=FALSO) Entao
    msg("ATENÇÃO: Tabela de Parcelas e/ou Clientes está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    Repita
      LimpaTela
      bitzero("LISTAGEM DE CLIENTES")
      Escreva("Codigo do cliente? >>ENTER<< Para todos: ")
      Leia(cli)
      acli<-verificar_cliente(cli)
      Se (acli=0) e (cli<>"") Entao
        msg("Cliente Inexistente. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
      FimSe
    Ate (acli<>0) ou (cli="")

    Repita
      Escreva("Informe o Código do banco ou >>ENTER<< Para todos: ")
      Leia(ban)
      aban<-verificar_banco(ban)
      Se (aban=0) e (ban<>"") Entao
        msg("BANCO Inexistente. Pressione >>ENTER<<",VERDADEIRO,VERDADEIRO)
        bitzero("LISTAGEM DE CLIENTES")
        Escreva("Codigo do cliente? >>ENTER<< Para todos: ",cli)
      FimSe
    Ate (aban<>0) ou (ban="")

    Repita
      Escreva("Parcelas em (A)berto ou (R)ecebidas ou >>ENTER<< Para todas: ")
      Leia(par)
    Ate (par="A") ou (par="a") ou (par="R") ou (par="r") ou (par="")
    Se (par="A") ou (par="a") Entao
      a<-"ABERTO:   "
    FimSe
    Se (par="R") ou (par="r") Entao
      a<-"RECEBIDO:"
    FimSe
    Se (par="") Entao
      a<-"GERAL:    "
    FimSe

    Repita
      Escreva("Ordem por (1)-Parcela (2)-Vencimento (3)-Nome: ")
      Leia(ord)
      Se (ord="") Entao
        ord<-"2"
      FimSe
    Ate (ord="1") ou (ord="2") ou (ord="3")

    Para y de 1 Ate totalcli faca
      Para x de 1 Ate totalpar faca
        Se (CLIENTE[y,1]=PARCELA[x,2]) e (CLIENTE[x,6]="=") e (PARCELA[x,7]="=") Entao//Se codigo cliente=codigo parcela e status do cliente e status da parcela verdadeira
          NomeParce[x]<-CLIENTE[y,2] //VETOR recebe nome do cliente
        FimSe
      FimPara
    FimPara
    class_parcela(ord)

    LimpaTela
    Para x de 1 Ate totalpar Faca
      Se (PARCELA[x,4]<>"") Entao
        diasqnt<-contadordias(PARCELA[x,3],PARCELA[x,4])
      SeNao
        diasqnt<-contadordias(PARCELA[x,3],dataatual)
      FimSe
      Se ((cli=PARCELA[x,2]) ou (cli="")) e ((ban=PARCELA[x,6]) ou (ban="")) e (((par="A") e (PARCELA[x,4]="")) ou ((par="R") e (PARCELA[x,4]<>"")) ou (par="")) e (PARCELA[x,7]="=") Entao
        Se lin=0 Entao
          EscrevaL("   PARCELA  CODIGO NOME                VENCIM.   RECEBI.  ATRASO  BANCO  VALOR")
          EscrevaL("-------------------------------------------------------------------------------------------")
        FimSe
        EscrevaL(x:3," ",PARCELA[x,1]:8," ",PARCELA[x,2]:5," ",NomeParce[x]:17,"   ",PARCELA[x,3]:8,"  ",PARCELA[x,4]:8," ",diasqnt:4,"    ",PARCELA[x,6]:4,"  ",PARCELA[x,5])
        lin<-lin+1
        Se lin=20 Entao
          EscrevaL("----------------------------------------------------------------------------------------")
          EscrevaL("DATA ATUAL: ",dataatual)
          msg("< Pressione >>ENTER<< Para continuar >",VERDADEIRO,VERDADEIRO)
          lin<-0
        FimSe
      FimSe
    FimPara
    Se lin>0 Entao
      EscrevaL("----------------------------------------------------------------------------------------------")
      EscrevaL("DATA ATUAL: ",dataatual,"                                  TOTAL ",a,TotalBancoSoma(a):10:2)
      msg("< Pressione >>ENTER<< Para retornar ao Menu >",VERDADEIRO,VERDADEIRO)
    FimSe
  FimSe
FimProcedimento

//=============================================================================================================================

Funcao TotalBancoSoma(condicao:CARACTER):real
VAR
  v1,v2:VETOR[1..6] de REAL
  t1,t2:REAL
    TMP:VETOR[1..80] de REAL
INICIO
  Se (totalpar>0) Entao
    Para x de 1 Ate totalpar Faca
      tmp[x]<-CaracPNum(PARCELA[x,5])
      Para y de 1 Ate 6 Faca
        Se (PARCELA[x,6]=carteiracod[y]) Entao
          Se (PARCELA[x,4]="") Entao
            v1[y]<-v1[y]+TMP[x]
            t1<-t1+TMP[x]
          SeNao
            v2[y]<-v2[y]+TMP[x]
            t2<-t2+TMP[x]
          FimSe
        FimSe
      FimPara
    FimPara
  FimSe

  Se (condicao="ABERTO:   ") Entao
    Retorne t1
  FimSe
  Se (condicao="RECEBIDO:") Entao
    Retorne t2
  FimSe
  Se (condicao="GERAL:    ") Entao
    Retorne (t1+t2)
  FimSe
FimFuncao

//=============================================================================================================================

Procedimento ResumoSinteticoBanco
VAR
  v1,v2:VETOR[1..6] de REAL
  t1,t2:REAL
    TMP:VETOR[1..80] de REAL
INICIO
  Se (importpar=FALSO) Entao
    msg("ATENÇÃO: Tabela de Parcelas está vazio. Pressione >>ENTER<< Para Voltar",VERDADEIRO,VERDADEIRO)
  SeNao
    bitzero("Resumo Sintético por Banco")
    EscrevaL("COD BANCO                      EM ABERTO   RECEBIDO      TOTAL")
    EscrevaL("--------------------------------------------------------------")
    Para x de 1 Ate totalpar Faca
      tmp[x]<-CaracPNum(PARCELA[x,5])
      Para y de 1 Ate 6 Faca
        Se (PARCELA[x,6]=carteiracod[y]) Entao
          Se (PARCELA[x,4]="") Entao
            v1[y]<-v1[y]+TMP[x]
            t1<-t1+TMP[x]
          SeNao
            v2[y]<-v2[y]+TMP[x]
            t2<-t2+TMP[x]
          FimSe
        FimSe
      FimPara
    FimPara
    Para y de 1 Ate 6 Faca
      Se (y=1) Entao
        EscrevaL(" ",carteiracod[y]:2," ",carteiranom[y]:25," ",v1[y]:10:2," ",v2[y]:10:2," ",v1[y]+v2[y]:10:2)
      SeNao
        EscrevaL(carteiracod[y]:3," ",carteiranom[y]:25," ",v1[y]:10:2," ",v2[y]:10:2," ",v1[y]+v2[y]:10:2)
      FimSe
    FimPara
    EscrevaL("--------------------------------------------------------------")
    EscrevaL("           T O T A I S .....: ",t1:10:2," ",t2:10:2," ",t1+t2:10:2)
    EscrevaL
    EscrevaL
    msg("Pressione >>ENTER<< Para retornar ao Menu.",VERDADEIRO,VERDADEIRO)
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento TiposBancos
VAR
  a:CARACTER
INICIO
  Repita
    bitzero("TIPOS DE BANCOS DISPONÍVEIS NO SISTEMA")
    EscrevaL(" ------------------------------------------")
    Para x de 1 Ate 6 faca
      Se (x<>1) Entao
        EscrevaL(" ","|","   ",carteiracod[x]:6,carteiranom[x]:31,"|")
      SeNao
        EscrevaL(" ","|","    ",carteiracod[x]:5,carteiranom[x]:31,"|")
      FimSe
    FimPara
    EscrevaL(" ------------------------------------------")
    EscrevaL
    msg(" PRESSIONE >>ENTER<< Para VOLTAR",VERDADEIRO,VERDADEIRO)
  Ate (a="")
FimProcedimento

//=============================================================================================================================
//========== FRONT END & OUTROS ===============================================================================================
//=============================================================================================================================

Procedimento msg(frase:CARACTER; aguarda:LOGICO; limpa:LOGICO)
VAR
  Parada:CARACTER
INICIO
  EscrevaL(frase)
  Se (aguarda=VERDADEIRO) Entao
    Leia(Parada)
  FimSe
  Se (limpa=VERDADEIRO) Entao
    LimpaTela
  FimSe
FimProcedimento

//=============================================================================================================================

Funcao Confirma(s:CARACTER):CARACTER
VAR
  x:CARACTER
INICIO
  Escreva(s)
  Leia(x)
  Retorne(x)
FimFuncao

//=============================================================================================================================

Procedimento bitzero(fraSe:CARACTER)
VAR
    a:INTEIRO
  b,c:CARACTER
INICIO
  LimpaTela
  EscrevaL(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  EscrevaL("                              B  I  T  Z  E  R  O")
  EscrevaL(" <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ")
  EscrevaL
  EscrevaL
  Escreva(">>>>> ")
  Para a de 1 Ate Compr(fraSe) Faca
    b<-COPIA(fraSe,a,1)
    c<-maiusc(b)
    Escreva(c)
    Escreva (" ")
  FimPara
  Escreva(" <<<<<")
  EscrevaL
  EscrevaL
  EscrevaL
FimProcedimento

//=============================================================================================================================

Procedimento Mostra_Cliente_Banco(pa:CARACTER;cli:CARACTER;acli:INTEIRO)
INICIO
  bitzero("INCLUIR PARCELA")
  EscrevaL("Código da parcela: ",pa:28," ",carteiracod[1]:2,": ",carteiranom[1])
  EscrevaL("Código do cliente: ",cli:28,carteiracod[2]:3,": ",carteiranom[2])
  EscrevaL("             Nome: ",CLIENTE[acli,2]:28,carteiracod[3]:3,": ",carteiranom[3])
  EscrevaL("           Cidade: ",CLIENTE[acli,3]:28,carteiracod[4]:3,": ",carteiranom[4])
  EscrevaL("               UF: ",CLIENTE[acli,4]:28,carteiracod[5]:3,": ",carteiranom[5])
  EscrevaL("         CPF/CNPJ: ",CLIENTE[acli,5]:28,carteiracod[6]:3,": ",carteiranom[6])
  EscrevaL(" =============================================")
FimProcedimento

//=============================================================================================================================

Procedimento Mostra_cliente(i:INTEIRO)
INICIO
  EscrevaL("             Nome: ",CLIENTE[i,2])
  EscrevaL("           Cidade: ",CLIENTE[i,3])
  EscrevaL("               UF: ",CLIENTE[i,4])
  EscrevaL("         CPF/CNPJ: ",CLIENTE[i,5])
  EscrevaL
  EscrevaL("------------------------------------------------------------------")
FimProcedimento

//=============================================================================================================================

Procedimento Mostra_Manutencao_Parcela(par,y2:CARACTER;apar,typo:INTEIRO)
INICIO
  bitzero("Manutenção de Parcela")
  EscrevaL ("Código da Parcela: ",par)
  Se (typo=1) Entao
    Mostra_Parcela(apar)
  SeNao
    Mostra_Parcela2(apar)
  FimSe
  EscrevaL
  EscrevaL("1 - Alterar Cliente")
  EscrevaL("2 - Alterar Banco")
  EscrevaL("3 - Alterar Valor")
  EscrevaL("4 - Alterar Data Vencimento")
  EscrevaL("5 - Lançar Recebimento")
  EscrevaL("6 - Excluir")
  EscrevaL("9 - Retornar")
  EscrevaL
  EscrevaL("Opção: ",y2)
  EscrevaL
FimProcedimento

//=============================================================================================================================

Procedimento Mostra_Parcela(i:INTEIRO)
INICIO
  EscrevaL("   Código Cliente: ",PARCELA[i,2])
  EscrevaL("     Nome Cliente: ",CLIENTE[verificar_cliente(PARCELA[i,2]),2])
  EscrevaL("            Banco: ",PARCELA[verificar_banco(PARCELA[i,6]),6])
  EscrevaL("            Valor: ",PARCELA[i,5])
  EscrevaL("  Data Vencimento: ",PARCELA[i,3])
  EscrevaL(" Data Recebimento: ",PARCELA[i,4])
  EscrevaL
  EscrevaL("------------------------------------------------------------------")
FimProcedimento

//=============================================================================================================================

Procedimento Mostra_Parcela2(i:INTEIRO)
INICIO
  EscrevaL("   Código Cliente: ",PARCELA[i,2]:28," ",carteiracod[1]:2,": ",carteiranom[1])
  EscrevaL("     Nome Cliente: ",CLIENTE[verificar_cliente(PARCELA[i,2]),2]:28,carteiracod[2]:3,": ",carteiranom[2])
  EscrevaL("            Banco: ",PARCELA[verificar_banco(PARCELA[i,6]),6]:28,carteiracod[3]:3,": ",carteiranom[3])
  EscrevaL("            Valor: ",PARCELA[i,5]:28,carteiracod[4]:3,": ",carteiranom[4])
  EscrevaL("  Data Vencimento: ",PARCELA[i,3]:28,carteiracod[5]:3,": ",carteiranom[5])
  EscrevaL(" Data Recebimento: ",PARCELA[i,4]:28,carteiracod[6]:3,": ",carteiranom[6])
  EscrevaL
  EscrevaL("------------------------------------------------------------------")
FimProcedimento

//=============================================================================================================================

Procedimento Mostra_editado_sucesso(tmp:CARACTER; apar,typo:INTEIRO)
INICIO
  conf<-confirma("Confirma Alteração (S/N)? ")
  Se (conf="S") ou (conf="s") Entao
    PARCELA[apar,typo]<-tmp
    EscrevaL
    EscrevaL("PARCELA EDITADA COM SUCESSO!")
    msg("Pressione >>ENTER<< Para continuar..",VERDADEIRO,VERDADEIRO)
  FimSe
FimProcedimento

//=============================================================================================================================

Procedimento InformeData
VAR
  dat:LOGICO
INICIO
  Repita
    Escreva("Informe a Data Atual (DD/MM/AA): ")
    Leia(dataatual)
    dat<-verificar_data(dataatual)
    Se (dat=FALSO) Entao
      msg("DATA INVÁLIDA. PRESSIONE >>ENTER<< Para INFORMAR OUTRA DATA",VERDADEIRO,VERDADEIRO)
    FimSe
  Ate (dat=VERDADEIRO)
FimProcedimento

//=============================================================================================================================

Funcao contadordias(datave,datare:CARACTER) :INTEIRO
VAR
  diaa,diav,mesa,mesv,anoa,anov,contador:INTEIRO
INICIO
  diaa<-CaracPNum(COPIA(datare,1,2))
  mesa<-CaracPNum(COPIA(datare,4,2))
  anoa<-CaracPNum(COPIA(datare,7,2))
  diav<-CaracPNum(COPIA(datave,1,2))
  mesv<-CaracPNum(COPIA(datave,4,2))
  anov<-CaracPNum(COPIA(datave,7,2))
  contador<-0
  Se (COPIA(datave,7,2))+(COPIA(datave,4,2))+(COPIA(datave,1,2)) >= (COPIA(datare,7,2))+(COPIA(datare,4,2))+(COPIA(datare,1,2)) Entao
    Retorne 0
  SeNao
    Repita
      contador<-contador+1
      diav<-diav+1
      Se (mesv<>2) Entao
        Se (diav=31) e ((mesv=4) ou (mesv=6) ou (mesv=9) ou (mesv=11)) Entao
          diav<-1
          mesv<-mesv+1
        FimSe
        Se (diav=32) Entao
          diav<-1
          mesv<-mesv+1
          Se (mesv=13) Entao
            mesv<-1
            anov<-anov+1
          FimSe
        FimSe
      SeNao
        Se (diav=29) e (anov MOD 4<>0) Entao
          diav<-1
          mesv<-mesv+1
        FimSe
        Se (diav=30) Entao
          diav<-1
          mesv<-mesv+1
        FimSe
      FimSe
    Ate (diaa=diav) e (mesa=mesv) e (anoa=anov)
    Retorne contador
  FimSe
FimFuncao

//=============================================================================================================================
//========== MENUS ============================================================================================================
//=============================================================================================================================

Procedimento MenuPrincipal     //MENU PRINCIPAL
VAR
  y2:CARACTER
INICIO
  Repita
    LimpaTela
    bitzero("MENU PRINCIPAL")
    EscrevaL("DATA ATUAL: ",dataatual)
    EscrevaL
    EscrevaL("1 - CLIENTES")
    EscrevaL("2 - PARCELAS")
    EscrevaL("3 - CONSULTA")
    EscrevaL("4 - CREDITOS DO PROGRAMA")
    Se (importcli=FALSO) ou (importpar=FALSO) Entao
      EscrevaL ("8 - IMPORTAR CLIENTES E PARCELAS")
    FimSe
    EscrevaL("9 - FIM")
    EscrevaL
    EscrevaL
    Escreva("Escolha: ")
    Leia(y2)
    Escolha y2
      Caso "1"
        LimpaTela
        SubMenu_Clientes
      Caso "2"
        LimpaTela
        SubMenu_Parcelas
      Caso "3"
        LimpaTela
        SubMenu_Consulta
      Caso "4"
        LimpaTela
        SubMenu_Creditos
      Caso "8"
        Se (importcli=FALSO) Entao
          LeCliente
          LimpaTela
        FimSe
        Se (importpar=FALSO) Entao
          LeParcela
          LimpaTela
        FimSe
    FimEscolha
  Ate (y2="9")
FimProcedimento

//=============================================================================================================================

Procedimento SubMenu_Clientes     //clientes 1
VAR
  y2:CARACTER
INICIO
  Repita
    bitzero("CLIENTES")
    EscrevaL("DATA ATUAL: ",dataatual)
    EscrevaL
    EscrevaL ("1 - CONSULTAR")
    EscrevaL ("2 - INCLUIR")
    EscrevaL ("3 - ALTERAR/EXCLUIR")
    Se (importcli=FALSO) Entao
      EscrevaL ("8 - IMPORTAR CLIENTES")
    FimSe
    EscrevaL ("9 - RETORNAR")
    EscrevaL
    Escreva ("Opcao: ")
    Leia(y2)
    Escolha y2
      Caso "1"
        LimpaTela
        ConsultaCliente
      Caso "2"
        LimpaTela
        InclusaoCliente
      Caso "3"
        LimpaTela
        ManutencaoCliente
      Caso "8"
        Se (importcli=FALSO) Entao
          LeCliente
          LimpaTela
        FimSe
    FimEscolha
  Ate (y2="9")
FimProcedimento

//=============================================================================================================================

Procedimento SubMenu_Parcelas    //parcelas 2
VAR
  y2:CARACTER
INICIO
  Repita
    bitzero("PARCELAS")
    EscrevaL("DATA ATUAL: ",dataatual)
    EscrevaL
    EscrevaL ("1 - CONSULTAR")
    EscrevaL ("2 - INCLUIR")
    EscrevaL ("3 - ALTERAR/EXCLUIR")
    Se (importpar=FALSO) Entao
      EscrevaL ("8 - IMPORTAR PARCELAS")
    FimSe
    EscrevaL ("9 - RETORNAR")
    EscrevaL
    Escreva ("Opcao: ")
    Leia(y2)
    Escolha y2
      Caso "1"
        LimpaTela
        ConsultaParcela
      Caso "2"
        LimpaTela
        InclusaoParcela
      Caso "3"
        LimpaTela
        ManutencaoParcela
      Caso "8"
        Se (importpar=FALSO) Entao
          LeParcela
          LimpaTela
        FimSe
    FimEscolha
  Ate (y2="9")
FimProcedimento

//=============================================================================================================================

Procedimento SubMenu_Consulta   //Consulta 3
VAR
  y2:CARACTER
INICIO
  Repita
    bitzero("CONSULTAS")
    EscrevaL("DATA ATUAL: ",dataatual)
    EscrevaL
    EscrevaL ("1 - CLIENTE")
    EscrevaL ("2 - PARCELAS")
    EscrevaL ("3 - BANCO")
    EscrevaL ("9 - RETORNAR")
    EscrevaL
    Escreva ("Opcao: ")
    Leia(y2)
    Escolha y2
      Caso "1"
        LimpaTela
        ConsultaCliente
      Caso "2"
        LimpaTela
        ConsultaParcela
      Caso "3"
        LimpaTela
        SubMenu_Banco
    FimEscolha
  Ate (y2="9")
FimProcedimento

//=============================================================================================================================

Procedimento SubMenu_Banco   //Consulta Banco 4
VAR
  y2:CARACTER
INICIO
  Repita
    bitzero("CONSULTAS")
    EscrevaL("DATA ATUAL: ",dataatual)
    EscrevaL
    EscrevaL ("1 - RESUMO SINTETICO BANCO")
    EscrevaL ("2 - TIPOS DE BANCOS")
    EscrevaL ("9 - RETORNAR")
    EscrevaL
    Escreva ("Opcao: ")
    Leia(y2)
    Escolha y2
      Caso "1"
        LimpaTela
        ResumoSinteticoBanco
      Caso "2"
        LimpaTela
        TiposBancos
    FimEscolha
  Ate (y2="9")
FimProcedimento

//=============================================================================================================================

Procedimento SubMenu_Creditos //SubMenu creditos
VAR
   cred:VETOR[1..16] de CARACTER
  k,s,v:INTEIRO
      c:CARACTER
INICIO
  bitzero ("Creditos")
  Cred[1]<-"                                 "
  Cred[2]<-"GOSTARIAMOS DE AGRADECER A TODOS ENVOLVIDOS NO PROJETO "
  Cred[3]<-"SEM A CONTRIBUIÇAO DE TODOS NAO TERIAMOS CHEGADO ATE AQUI."
  Cred[4]<-                            "
  Cred[5]<-" >> ALGO SO E IMPOSSIVEL ATE QUE ALGUEM DUVIDE E RESOLVA <<"
  Cred[6]<-"                  >> PROVAR AO CONTRARIO! <<"
  Cred[7]<-"                     - Albert Einsten -"
  Cred[8]<-"                            "
  Cred[9]<-"   ORIENTADOR: ALEXANDRE MORENO"
  Cred[10]<-"PRODUCT OWNER: ALAN MINORU OTANI"
  Cred[11]<-" SCRUM MASTER: RODRIGO BRANCO NASCIMENTO"
  Cred[12]<-"    DEV. TEAM: ALLAN OGAWA"
  Cred[13]<-"    DEV. TEAM: HEITOR BRANDOLIN"
  Cred[14]<-"    DEV. TEAM: MICHELLE FREGNANI OTANI"
  Cred[15]<-"                          "
  Cred[16]<-"                          "
  Para x de 1 Ate 16 Faca
    Para y de 1 Ate Compr(Cred[x]) Faca
      c<-COPIA(Cred[x],y,1)
      Escreva(c)
      Para v de 1 Ate 700 Faca
        k<-k+v
      FimPara
    FimPara
    EscrevaL
  FimPara
  msg("Pressione >>ENTER<< Para retornar",VERDADEIRO,VERDADEIRO)
FimProcedimento

//=============================================================================================================================
//========== PROGRAMA =========================================================================================================
//=============================================================================================================================

INICIO
  InformeData
  importcli<-FALSO
  importpar<-FALSO
  CarregaBancos
  LimpaTela
  MenuPrincipal
FimAlgoritmo
