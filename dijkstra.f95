program dijstra
    implicit none  ! evitar erro com antigas versoes do fortran
    ! exemplo de uso: ./a.out matriz-100.dat 1 60
    integer, dimension(:,:), allocatable :: matrix
    integer, dimension(:), allocatable :: caminho
    integer :: i
    integer :: j
    integer :: cont
    integer :: ordem
    integer :: max_cont
    integer :: argc
    integer :: origem
    integer :: atual
    integer :: destino
    integer :: menor
    integer :: estado_entrada
    integer :: erro_leitura
    character(255) :: nome_arquivo
    character(2000) :: bife  ! buffer para leitura do arquivo (file?!!)
    character(7) :: buffer_argv  ! buffer para pegar os argumentos 
    integer, parameter :: ID_ARQUIVO = 110592
    integer, parameter :: INFINITO = 1000000  ! quem disse que nao da para saber quanto eh infinito?
    integer, parameter :: FALSE = 0
    integer, parameter :: TRUE = 1
    
    TYPE :: No  ! Tipo assim, um No
        integer :: distancia
        integer :: antecessor
        integer :: processado
    END TYPE No
    
    TYPE(No), dimension(:), allocatable :: nos
    TYPE(No), parameter :: NO_NULO = No(INFINITO, -1, FALSE)
    
    !print *,'Instituto Federal de Educacao, Ciencia e Tecnologia do Sudeste de Minas Gerais'
    !print *,'Curso Tecnologia em Sistemas para Internet'
    !print *,'Aluno: Arthur Assuncao'
    !print *,'TCP e Roteamento'
    !print *,'Algoritmo Dijkstra em Fortran 95'
    !print *,''
    
    ! Pega os argumentos origem e destino
    argc = iargc()
    if(argc .lt. 3) then  !tem que ter dois argumentos
        print *, 'Falta argumento'
        return
    end if
    ! le o primeiro argumento, o endereco do arquivo da matriz
    call getarg(1, nome_arquivo)
    ! le o segundo argumento, a origem
    call getarg(2, buffer_argv)
    read(buffer_argv, '(i8)') origem
    ! le o terceiro argumento, o destino
    call getarg(3, buffer_argv)
    read(buffer_argv, '(i8)') destino
    ordem = -1
    if(argc .eq. 4) then  ! tem a ordem
        call getarg(4, buffer_argv)
        read(buffer_argv, '(i8)') ordem 
    end if
    
    ! le a matriz
    open(unit=ID_ARQUIVO, file=nome_arquivo, status='old', iostat=estado_entrada, access='sequential', form='formatted')
    read(ID_ARQUIVO, '(A)'), bife
    if(ordem .eq. -1) then
        ! lendo a primeira linha
        cont = 0
        if(bife(1:1) .ne. ' ') then
            ordem = 1  !comeca com numero
        else
            ordem = 0  !comeca com espaco
        end if
        ! pega o tamanho da linha
        max_cont = len_trim(bife)
        ! conta o numero de numeros da primeira linha, por ser uma matriz quadrada este valor representa o numero de linhas do arquivo, assim linha x coluna temos a ordem da matriz
        do
            cont = cont + 1
            if(bife(cont:cont) .eq. ' ' .and. bife(cont+1:cont+1) .ne. ' ') then
                ordem = ordem + 1  ! eh, tem mais numero
            end if
            if(cont > max_cont) then
                exit
            end if
        end do
    end if
    print '(A,i4)','matriz de ordem: ', ordem
    cont = cont - 1
    ! verifica se origem e destino sao validos
    if(origem .gt. ordem .or. destino .gt. ordem) then
        print *,'origem ou destino invalido'
        return
    end if
    ! aloca espaco para a matriz
    allocate(matrix(ordem, ordem))
    ! volta para a primeira linha do arquivo
    rewind(ID_ARQUIVO)
    ! le a p*rra toda
    read(ID_ARQUIVO, *) matrix
    
    !print *,''
    ! fecha o arquivo
    close(ID_ARQUIVO)
    
    ! dijkstra is coming
    menor = INFINITO !menor caminho o.O
    ! aloca N Nos, onde N eh ordem
    allocate(nos(ordem))
    nos = NO_NULO ! inicializa todo o vetor de nos
    
    nos(origem) = No(0, -1, TRUE)
    
    ! buscando o menor caminho
    atual = origem
    j = 0
    do while (atual .ne. destino)
        do i = 1,ordem
            if (matrix(atual, i) .ne. 0 .and. nos(i)%processado .ne. TRUE) then
                if (nos(atual)%distancia + matrix(atual, i) .lt. nos(i)%distancia) then
					nos(i)%antecessor = atual
					nos(i)%distancia = nos(atual)%distancia + matrix(atual, i)
				end if
			end if
		end do
		menor = INFINITO
        j = 0
		do i = 1,ordem
		    if (nos(i)%processado .eq. FALSE .and. nos(i)%distancia .lt. menor) then
				menor = nos(i)%distancia
				atual = i
                j = i
			end if
		end do
        if(j .eq. 0) exit  ! se o J continua 0 eh pq o No esta isolado
		nos(atual)%processado = TRUE
    end do
    deallocate(matrix)
    
    ! resultados
    !print *,''
	print '(A,i4)','Origem: ', origem
	print '(A,i4)','Destino: ', destino
	print '(A,i4)','Custo: ', nos(destino)%distancia

    ! construindo o caminho
	atual = destino
	j = 1
	allocate(caminho(ordem))
	caminho = -1 ! inicia tudo com 0
	! constroe o caminho
	do while (atual .ne. -1)
    	caminho(j) = atual
		atual = nos(atual)%antecessor
		j = j + 1
	end do
	j = j - 1
	print *,'Caminho: ', caminho(j:1:-1)  ! notacao, no minimo, exotica para inverter o vetor
    deallocate(caminho)
    deallocate(nos)
    
end program dijstra


