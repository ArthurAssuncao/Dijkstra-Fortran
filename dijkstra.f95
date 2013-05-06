program dijstra
    integer, dimension(:,:), allocatable :: matrix
    integer, dimension(:), allocatable :: caminho
    integer :: i, j, cont, ordem, max_cont, origem, atual, destino, menor
    integer :: estado_entrada, erro_leitura
    character(18), parameter :: NOME_ARQUIVO_MENOR = "matriz_pequena.dat"
    character(14), parameter :: NOME_ARQUIVO_TESTE = "matriz-100.dat"
    character(2000) :: bife
    integer, parameter :: ID_ARQUIVO = 110592
    integer, parameter :: INFINITO = 1000000 ! quem disse que nao da para saber quanto eh infinito?
    integer, parameter :: FALSE = 0
    integer, parameter :: TRUE = 1
    
    TYPE :: No ! Tipo assim, um No
        integer :: distancia
        integer :: antecessor
        integer :: processado
    END TYPE No
    
    TYPE(No), dimension(:), allocatable :: nos
    TYPE(No), parameter :: NO_NULO = No(INFINITO, -1, FALSE)
    
    print *,'Instituo Federal do Sudeste de Minas Gerais'
    print *,'Curso Tecnologia em Sistemas para Internet'
    print *,'Aluno: Arthur Assuncao'
    print *,'TCP e Roteamento'
    print *,'Algoritmo Dijkstra em Fortran'
    print *,''
    
    open(unit=ID_ARQUIVO, file=NOME_ARQUIVO_TESTE, status='old', iostat=estado_entrada, access='sequential', form='formatted')
    read(ID_ARQUIVO, '(A)'), bife
    ! lendo a primeira linha
    cont = 0
    if(bife(1:1) .ne. ' ') then
        ordem = 1 !comeca com numero
    else
        ordem = 0 !comeca com espaco
    end if
    max_cont = len(trim(bife))
    do
        cont = cont + 1
        if(bife(cont:cont) .eq. ' ' .and. bife(cont+1:cont+1) .ne. ' ') then
            ordem = ordem + 1
        end if
        if(cont > max_cont) then
            exit
        end if
    end do
    print *,'matrix de ordem: ', ordem
    cont = cont - 1
    allocate(matrix(ordem, ordem))
    rewind(ID_ARQUIVO)
    read(ID_ARQUIVO, *) matrix
    
    print *,''
    !write(*,*) matrix
    close(ID_ARQUIVO)
    
    ! dijkstra is coming
    origem = 1 ! pegar como argumento
    destino = 9
    menor = INFINITO !menor caminho
    allocate(nos(ordem))
    nos = NO_NULO ! inicializa todo o vetor de nos
    
    nos(origem) = No(0, -1, TRUE)
    
    ! buscando o menor caminho
    atual = origem
    do while (atual .ne. destino)
        do i = 1,ordem
            if (matrix(atual, i) .ne. 0 .and. nos(i)%processado .ne. TRUE) then
                if (nos(atual)%distancia + matrix(atual, i) .lt. nos(i)%distancia) then
					nos(i)%antecessor = atual
					nos(i)%distancia = nos(atual)%distancia + matrix(atual, i)
				end if
			end if
		!2 continue
		end do
		menor = INFINITO
		do i = 1,ordem
		    if (nos(i)%processado .eq. FALSE .and. nos(i)%distancia .lt. menor) then
				menor = nos(i)%distancia
				atual = i
			end if
		!3 continue
		end do
		nos(atual)%processado = TRUE
    end do
    
    ! resultados
    print *,''
	print *,'Origem: ', origem
	print *,'Destino: ', destino
	print *,'Custo: ', nos(destino)%distancia

    ! construindo o caminho
	atual = destino
	j = 1
	allocate(caminho(ordem))
	caminho = -1 ! inicia tudo com 0
	do while (atual .ne. -1)
    	caminho(j) = atual
		atual = nos(atual)%antecessor
		j = j + 1
	end do
	j = j - 1
	print *,'Caminho: ', caminho(5:1:-1)
    
end program dijstra

