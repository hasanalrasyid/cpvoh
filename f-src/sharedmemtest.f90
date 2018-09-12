program sharedmemtest
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER
  use mpi
  implicit none
  integer, parameter :: dp = selected_real_kind(14,200)
  integer :: win,hostcomm,procRankInHost,totalHostComm
  integer :: hostRankInWorld
  integer :: disp_unit,procRankInWorld,ierr,TotalWorld
  integer :: sleeptime,testsize
  integer :: i,j,k
  integer :: ip,jp,ilp,jlp,ieP,myunit
  character(len=30) :: rowfmt
  INTEGER(KIND=MPI_ADDRESS_KIND) :: windowsize
  TYPE(C_PTR) :: baseptr,baseptr2
  real(dp), POINTER :: matrix_elementsy(:,:)
  real(dp), POINTER :: matrix_elementsy_single(:)
  !real(dp), POINTER :: matrix_elementsy(:,:,:,:)
  integer,allocatable :: arrayshape(:),arrayshape1(:)

  call MPI_INIT( ierr )

  !GET THE RANK OF ONE PROCESS
  call MPI_COMM_RANK(MPI_COMM_WORLD,procRankInWorld,IERR)
  !GET THE TOTAL PROCESSES OF THE COMM
  call MPI_COMM_SIZE(MPI_COMM_WORLD,totalWorld,IERR)
  !splits the communicator into subcommunicators, each of which can create a shared memory region.
  !output: hostcomm -> new subcomm based on MPI_COMM_TYPE_SHARED
  CALL MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, hostcomm,ierr)
  !check the rank of this host/worker based on new communicator hostcomm
  !output : procRankInHost -> this host rank
  CALL MPI_Comm_rank(hostcomm, procRankInHost,ierr)
  call MPI_COMM_SIZE(hostcomm,totalHostComm,ierr)
  ! Gratefully based on: http://stackoverflow.com/questions/24797298/mpi-fortran-code-how-to-share-data-on-node-via-openmp
  ! and https://gcc.gnu.org/onlinedocs/gfortran/C_005fF_005fPOINTER.html
  ! We only want one process per host to allocate memory
  ! Set size to 0 in all processes but one
  testsize=21
  allocate(arrayshape(2))
  allocate(arrayshape1(1))
  arrayshape=(/ testsize,testsize /)
  arrayshape1=(/ testsize*testsize /)
  !allocate(arrayshape(4))
  !arrayshape=(/ testsize,testsize,testsize,testsize /)
  if (procRankInHost == 0) then
     ! Put the actual data size here
     windowsize = int(testsize**2,MPI_ADDRESS_KIND)*8_MPI_ADDRESS_KIND !*8 for double
     !windowsize = int(testsize**4,MPI_ADDRESS_KIND)*8_MPI_ADDRESS_KIND !*8 for double
  else
     windowsize = 0_MPI_ADDRESS_KIND
  end if
  disp_unit = 1
  !memory allocation for sharing with other in the sample hostcomm
  ! input:
  ! windowsize: total size of memory window
  ! disp_unit: local unit size for displacements in bytes
  !output:
  ! baseptr : initial address of the memory window
  ! win: window object returned by the call
  CALL MPI_Win_allocate_shared(windowsize, disp_unit, MPI_INFO_NULL, hostcomm, baseptr, win, ierr)

  ! Obtain the location of the memory segment
  if (procRankInHost /= 0) then
     CALL MPI_Win_shared_query(win, 0, windowsize, disp_unit, baseptr, ierr)
  end if

  ! baseptr can now be associated with a Fortran pointer
  ! and thus used to access the shared data
  ! but... matrix_elementsy adalah whole matrix. bukan submatrix.
  ! kita harus implement sendiri submatrix allocation.
  CALL C_F_POINTER(baseptr, matrix_elementsy,arrayshape)
  !CALL C_F_POINTER(baseptr, matrix_elementsy_single,arrayshape1)

  !!! your code here!
  !!! sample below

  !coba alokasikan matrix_elementsy berdasarkan lokasi
     !matrix_elementsy=dble(procRankInHost)

  iP = 1 + (procRankInWorld * testsize / totalWorld)
  ilP =   ((1+procRankInWorld) * testsize/ totalWorld) &
        - (iP - 1)
  ieP = iP + ilP - 1
  jP = iP
  jlP = ilP
  print *,"my_rank===",procRankInWorld, iP, iP + ilP - 1
  do j = iP, ieP
    do i = 1, testsize
      matrix_elementsy(i,j) = procRankInWorld+(i*0.01)+(j*0.0001)
    enddo
  enddo
  CALL MPI_WIN_FENCE(0, win, ierr)
  !if (procRankInHost == 0) then
  !   matrix_elementsy(1,2)=1.0_dp
  !!matrix_elementsy(1,2,3,4)=1.0_dp
  !end if

  print 1,"my_rank=",procRankInWorld,procRankInHost, TotalWorld, totalHostComm, &
    hostcomm, matrix_elementsy(1,2),matrix_elementsy(1,5), windowsize
1 format(A10,I2,I2,I2,I2,I14,f7.2,f7.2,I6)
  !print *,"my_rank=",procRankInWorld,matrix_elementsy(1,2,3,4),matrix_elementsy(1,2,3,5), windowsize
  if (procRankInHost == 0) then
    myunit = 10 + procRankInWorld
    WRITE(rowfmt,'(A,I4,A)') '(',testsize,'(1X,F7.4))'
    OPEN(UNIT=myunit, ACTION="write")
    !OPEN(UNIT=12, FILE="aoutput.txt", ACTION="write", STATUS="replace", &
    !   RECL=(7*testsize+10))
    DO i=1,testsize
      WRITE(myunit,FMT=rowfmt) (matrix_elementsy(i,j), j=1,testsize)
    END DO
  CLOSE(UNIT=myunit)
  end if
  !call sleep(15)
  !!! end sample code

  call MPI_WIN_FENCE(0, win, ierr)
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  call MPI_Win_free(win,ierr)
  call MPI_FINALIZE(IERR)

  end program
