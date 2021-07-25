	.text			
	.globl	main	
		
main:           nop # instru��o coringa, j� que, por algum motivo que, no momento, nos � desconhecido, a primeira instru��o � ignorada
		lui $at, 0x00001001 # la $t3,tamanho (carrega o endere�o do tamanho dos vetores no registrador $t3)
		ori $t3, $at, 0x00000060 # la $t3,tamanho (carrega o endere�o do tamanho dos vetores no registrador $t3)
		lui $at, 0x00001001 # la $t4, dois (carrega o endere�o da constante '2' no registrador $t4)
		ori $t4, $at, 0x00000064 # la $t4, dois (carrega o endere�o da constante '2' no registrador $t4)
		lui $at, 0x00001001 # la $t0,ENT_1 (usa o registrador $t0 como ponteiro para o vetor ENT_1)
		ori $t0, $at, 0 # la $t0,ENT_1 (usa o registrador $t0 como ponteiro para o vetor ENT_1)
		lui $at, 0x00001001 # la $t1,ENT_2 (usa o registrador $t1 como ponteiro para o vetor ENT_2)  
		ori $t1, $at, 0x00000020  # la $t1,ENT_2 (usa o registrador $t1 como ponteiro para o vetor ENT_2)
		lui $at, 0x00001001 # la $t2,SAI (usa o registrador $t2 como ponteiro para o vetor SAI)
		ori $t2, $at, 0x00000040 # la $t2,SAI (usa o registrador $t2 como ponteiro para o vetor SAI)
		lw $t3,0($t3) # carrega o valor do tamanho dos vetores no registrador $t3
		lw $t4, 0($t4) # carrega a constante '2' no registrador $t4

	     
pre_verifica:  lw $t5, 0($t0) # carrega o valor que est� na posi��o do vetor ENT_1 no registador $t5
	           
		
verifica:	subu $t5, $t5, $t4 # faz a divis�o por subtra��es sucessivas
			beq $t5, $zero, pre_verifica_par # caso tenhamos "resto" 0, verifica se o pr�ximo � par
			lui $s0, 0x0000ffff # como n�o podemos usar -1 no beq (pois ele gera um addi)
			ori $s1, $s0, 0x0000ffff # faz parte do lui
			beq $t5, $s1, pre_verifica_impar # caso tenhamos "resto" -1, verifica se o pr�ximo � impar
			j verifica # repete o la�o at� chegarmos a 0 ou -1
	  

pre_verifica_impar: lw $t5, 0($t1) # carrega novamente no registrador $t5 o conte�do a ser analisado, dessa vez do vetor ENT_2
                                       
                    
verifica_impar: subu $t5, $t5, $t4 # faz a divis�o por subtra��es sucessivas
				beq $t5, $zero, subtracao # caso tenhamos "resto" 0, faz a subtracao
				beq $t5, $s1, soma  # caso tenhamos "resto" -1, faz a soma
				j verifica_impar # repete o la�o at� chegarmos a 0 ou -1
		
			        	   
pre_verifica_par: lw $t5, 0($t1) # carrega novamente no registrador $t5 o conte�do a ser analisado, dessa vez do vetor ENT_2
		  
		  
verifica_par: 	subu $t5, $t5, $t4     
				beq $t5, $zero, soma # caso tenhamos "resto" 0, faz a soma
				beq $t5, $s1, subtracao # caso tenhamos "resto" -1, faz a subtracao
				j verifica_par # repete o la�o at� chegarmos a 0 ou -1
	      

soma:   blez	$t3,fim		# caso o tamanho do vetor tenha chegado ao fim, desvia para o fim da execu��o 
		lw	$t5,0($t0)	# caso contr�rio, carrega o valor que est� na posi��o do vetor ENT_1
		lw	$t6,0($t1) 	# carrega o valor que est� na posi��o do vetor ENT_2
		addu	$t5,$t5,$t6 	# soma os dois vetores	
		sw	$t5,0($t2)	# escreve o resultado da soma na posi��o em mem�ria de SAI
		addiu	$t0,$t0,4	# atualiza o ponteiro para o pr�ximo elemento em ENT_1
		addiu	$t1,$t1,4	# atualiza o ponteiro para o pr�ximo elemento em ENT_2
		addiu	$t2,$t2,4	# atualiza o ponteiro para a pr�xima posi��o em SAI
		addiu	$t3,$t3,-1	# decrementa o n�mero de elementos analisados
		j	pre_verifica	# continua a execu��o do programa 
	
		

subtracao:	blez	$t3,fim		# caso o tamanho do vetor tenha chegado ao fim, desvia para o fim da execu��o
			lw	$t5,0($t0)	# caso contr�rio, carrega o valor que est� na posi��o do vetor ENT_1
			lw	$t6,0($t1) 	# carrega o valor que est� na posi��o do vetor ENT_2
			subu	$t5,$t5,$t6 	# subtrai os dois vetores
			sw	$t5,0($t2)	# escreve o resultado da subtra��o na posi��o em mem�ria de SAI
			addiu $t0,$t0,4	# atualiza o ponteiro para o pr�ximo elemento em ENT_1
			addiu $t1,$t1,4	# atualiza o ponteiro para o pr�ximo elemento em ENT_2
			addiu $t2,$t2,4	# atualiza o ponteiro para a pr�xima posi��o em SAI
			addiu $t3,$t3,-1	# decrementa o n�mero de elementos analisados
			j	pre_verifica	# continua a execu��o do programa 
	   
	   


fim:	li	$v0,10
		syscall			# fim do programa


	.data			
ENT_1:	.word	1 3 2 2 2 2 4 9 
ENT_2:	.word	1 2 3 4 7 5 1 3 
SAI:	.word	0 0 0 0 0 0 0 0

tamanho: .word	8	
dois:	 .word 	2	


