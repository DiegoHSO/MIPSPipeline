	.text			
	.globl	main	
		
main:   nop # instrução coringa, já que, por algum motivo que, no momento, nos é desconhecido, a primeira instrução é ignorada
		lui $at, 4097 # la $t1,tamanho_fat (usa o registrador $t1 como ponteiro para o vetor ENT_2)	
		ori $t1, $at, 24 # la $t1,tamanho_fat (usa o registrador $t1 como ponteiro para o vetor ENT_2)	
		lui $at, 4097 # la $t1,tamanho_fat (usa o registrador $t1 como ponteiro para o vetor ENT_2)
		ori $t0, $at, 0 # la $t0,resultado (usa o registrador $t0 como ponteiro para o vetor ENT_1)
		lw $t2,0($t1) # carrega o valor do número de fatoriais que serão feitos
		lw $t3,0($t1) # carrega mais uma vez o número de fatoriais que serão feitos
		lw $t6,0($t1) # carrega mais uma vez o número de fatoriais que serão feitos
		lw $t1,0($t1) # carrega mais uma vez o número de fatoriais que serão feitos
		addiu $t2, $t2, -1 # subtrai 1 do registrador $t2 (servirá como multiplicador do fatorial)
		addiu $t4, $zero, 1 # deixa a constante '1' no registrador $t4 
		addiu $t5, $zero, 1 # deixa a constante '1' no registrador $t4
		
	     	     
fat:   beq  $t1, $t4, fim # caso estejamos fazendo o fatorial de 1, desvia para o fim do programa
	   beq  $t2, $zero, proximo # caso o multiplicador tenha chegado a 0, desvia para o rótulo "proximo"
	   beq $t5, $t2, diminui_mult # caso o contador seja igual ao multiplicador, desvia
	   addu $t3, $t3, $t6 # faz a multiplicação em somas sucessivas
	   addiu $t5, $t5, 1 # incrementa o contador
	   j fat
	   
	   	   
diminui_mult:	addiu $t2, $t2, -1 # subtrai um no multiplicador
				addiu $t5, $zero, 1 # reinicializa o contador
				addu $t6, $t3, $zero # ajusta o multiplicando
				j fat # repete o processo


proximo:	addiu $t1, $t1, -1 # diminui 1 no contador de fatoriais restantes a serem feitos
			sw $t3, 0($t0) # registra na memória o resultado do fatorial
			addu $t2, $zero, $t1 # reseta o valor em $t2 com o valor do próximo fatorial a ser feito
			addu $t3, $zero, $t1 # reseta o valor em $t3 com o valor do próximo fatorial a ser feito
			addu $t6, $zero, $t1 # reseta o valor em $t3 com o valor do próximo fatorial a ser feito
			addiu $t5, $zero, 1 # reinicializa o contador
			addiu $t2, $t2, -1 # subtrai 1 no registrador $t2
			addiu $t0, $t0, 4 # move o ponteiro da memória para a próxima posição
			j fat

	  
fim:	sw $t1, 0($t0) # registra na memória o resultado do fatorial de 1, que é 1
		addiu $t0, $t0, 4 # move o ponteiro da memória para a próxima posição
		sw $t1, 0($t0) # registra na memória o resultado do fatorial de 0, que é 1
		li	$v0,10
		syscall			# fim do programa


	.data			
resultado:   .word	0x0 0x0 0x0 0x0 0x0 0x0

tamanho_fat: .word	5	


