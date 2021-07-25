	.text			
	.globl	main	
		
main:   nop # instru��o coringa, j� que, por algum motivo que, no momento, nos � desconhecido, a primeira instru��o � ignorada
		lui $at, 4097 # la $t0,ordenado (usa o registrador $t0 como ponteiro para o vetor original (vet[i]))
		ori $t0, $at, 80 # la $t0,ordenado (usa o registrador $t0 como ponteiro para o vetor original (vet[i]))
		lui $at, 4097 # la $t1,ordenado (usa mais um registrador, o $t1, como ponteiro para o vetor original)
		ori $t1, $at, 80 # la $t1,ordenado (usa mais um registrador, o $t1, como ponteiro para o vetor original)
		addiu $t1, $t1, 4 # aponta para a pr�xima posi��o do vetor (vet[i+1])
		lw $t2,0($t0) # carrega o valor na posi��o do vetor no registrador $t2
		lw $t3,0($t1) # carrega o valor na posi��o do vetor no registrador $t3
		addiu $t4, $zero, 1 # deixa a constante '1' no registrador $t4 (verificador do flag)
		lui $at, 4097 # la $t5, tamanho (armazena o endere�o do tamanho do vetor no registrador $t5)
		ori $t5, $at, 160 # la $t5, tamanho (armazena o endere�o do tamanho do vetor no registrador $t5)  
		lui $at, 4097 # lw $t5, tamanho (armazena o valor do tamanho do vetor no registrador $t5)
		lw $t5, 160($at) # lw $t5, tamanho (armazena o valor do tamanho do vetor no registrador $t5)
		addiu $t5, $t5, -1 # subtrai 1 no valor do tamanho do vetor (n-1 elementos)
		addiu $t7, $t7, 1 # inicializa o contador de �ndices do vetor analisados
		addiu $t8, $t8, 1 # inicializa o outro contador de �ndices do vetor analisados
		addu $t6, $t5, $zero # armazena o valor do tamanho do vetor em mais um registrador, o $t6
	     
	      
ordena:	beq $t6, $t8, proximo # caso o contador de �ndices analisados do vetor seja igual ao n�mero de �ndices 
								# dispon�veis para an�lise, desvia para o r�tulo "proximo"						
		slt $t9, $t3, $t2 # caso o conte�do em $t3 seja menor que o conte�do em $t2 (vet[i+1] < vet[i]),
	   									# atribui '1' ao registrador $t9 (flag)									
		beq  $t9, $t4, troca # caso o flag seja igual a '1', desvia para o r�tulo "troca"
		addiu $t1, $t1, 4 # aponta para a pr�xima posi��o do vetor
		addiu $t8, $t8, 1 # incrementa o contador de �ndices analisados
		addu $t9, $zero, $zero # reinicaliza o flag
		lw $t3, 0($t1) # carrega o valor da posi��o do vetor no registrador $t3
		j ordena # repete o la�o
	
	   
troca:	sw $t3, 0($t0) # armazena o conte�do de $t3 no endere�o armazenado em $t0 (vet[i] = vet[i+1])
		sw $t2, 0($t1) # armazena o conte�do de $t2 no endere�o armazenado em $t1 (vet[i+1] = vet[i])
		ori $at, $zero, 4 # la $t1, 4($t0) (aponta para a pr�xima posi��o do vetor (posi��o seguinte � apontada por $t0))
		addu $t1, $t0, $at # la $t1, 4($t0) (aponta para a pr�xima posi��o do vetor (posi��o seguinte � apontada por $t0))
		lw $t2, 0($t0) # carrega o valor da posi��o do vetor no registrador $t2
		lw $t3, 0($t1) # carrega o valor da posi��o do vetor no registrador $t3
		addu $t8, $zero, $zero # reinicializa o contador de �ndices dispon�veis para an�lise
		addu $t9, $zero, $zero # reinicaliza o flag
		j ordena # volta ao la�o que compara os valores no vetor


proximo:	beq $t5, $t7, fim # caso todos os elementos do vetor tenham sido analisados, desvia para o r�tulo de fim de programa
			addiu $t0, $t0, 4 # aponta para a pr�xima posi��o do vetor a ser comparada com o resto (posi��o seguinte � apontada por $t0)
			lw $t2, 0($t0) # carrega o valor da posi��o do vetor no registrador $t2
			ori $at, $zero, 4 # la $t1, 4($t0) (aponta para a pr�xima posi��o do vetor (posi��o seguinte � apontada por $t0))
			addu $t1, $t0, $at # la $t1, 4($t0) (aponta para a pr�xima posi��o do vetor (posi��o seguinte � apontada por $t0))
			lw $t3, 0($t1) # carrega o valor da posi��o do vetor no registrador $t3
			addu $t8, $zero, $zero # reinicializa o contador de �ndices dispon�veis para an�lise
			addu $t9, $zero, $zero # reinicaliza o flag
			addiu $t6, $t6, -1 # diminui uma unidade no contador de �ndices dispon�veis para an�lise
			addiu $t7, $t7, 1 # registra que mais um elemento do vetor foi totalmente analisado e comparado com os outros
			j ordena # volta ao la�o que compara os valores no vetor

	 	  
fim:	li	$v0,10
	syscall			# fim do programa


	.data			
original:   .word	0x12 0xff 0x3 0x14 0x878 0x31 0x62 0x10 0x5 0x16 0x20 0x30 0x8 0x50 0xa2 0x98 0x44 0x75 0x101 0x343
ordenado:   .word	0x12 0xff 0x3 0x14 0x878 0x31 0x62 0x10 0x5 0x16 0x20 0x30 0x8 0x50 0xa2 0x98 0x44 0x75 0x101 0x343
tamanho:    .word	20
