program sharedmemtest
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER
  use mpi
  implicit none
  integer, parameter :: dp = selected_real_kind(14,200)
  integer :: win,win2,hostcomm,hostrank
  integer :: sleeptime,testsize
  integer :: i,j,k
  character(len=30) :: rowfmt
  INTEGER(KIND=MPI_ADDRESS_KIND) :: windowsize
  INTEGER :: disp_unit,my_rank,ierr,total
  TYPE(C_PTR) :: baseptr,baseptr2
  real(dp), POINTER :: matrix_elementsy(:,:)
  !real(dp), POINTER :: matrix_elementsy(:,:,:,:)
  integer,allocatable :: arrayshape(:)

  call MPI_INIT( ierr )

  !GET THE RANK OF ONE PROCESS
  call MPI_COMM_RANK(MPI_COMM_WORLD,MY_RANK,IERR)
  !GET THE TOTAL PROCESSES OF THE COMM
  call MPI_COMM_SIZE(MPI_COMM_WORLD,Total,IERR)
  !splits the communicator into subcommunicators, each of which can create a shared memory region.
  !output: hostcomm -> new subcomm based on MPI_COMM_TYPE_SHARED
  CALL MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, hostcomm,ierr)
  !check the rank of this host/worker based on new communicator hostcom
  !output : hostrank -> this host rank
  CALL MPI_Comm_rank(hostcomm, hostrank,ierr)

  ! Gratefully based on: http://stackoverflow.com/questions/24797298/mpi-fortran-code-how-to-share-data-on-node-via-openmp
  ! and https://gcc.gnu.org/onlinedocs/gfortran/C_005fF_005fPOINTER.html
  ! We only want one process per host to allocate memory
  ! Set size to 0 in all processes but one
  testsize=36
  allocate(arrayshape(2))
  arrayshape=(/ testsize,testsize /)
  !allocate(arrayshape(4))
  !arrayshape=(/ testsize,testsize,testsize,testsize /)
  if (hostrank == 0) then
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
  if (hostrank /= 0) then
     CALL MPI_Win_shared_query(win, 0, windowsize, disp_unit, baseptr, ierr)
  end if

  ! baseptr can now be associated with a Fortran pointer
  ! and thus used to access the shared data
  ! but... matrix_elementsy adalah whole matrix. bukan submatrix.
  ! kita harus implement sendiri submatrix allocation.
  CALL C_F_POINTER(baseptr, matrix_elementsy,arrayshape)

  !!! your code here!
  !!! sample below

  !coba alokasikan matrix_elementsy berdasarkan lokasi
     !matrix_elementsy=dble(hostrank)
  if (hostrank == 0) then
     matrix_elementsy=0.0_dp
     matrix_elementsy(1,2)=1.0_dp
     !matrix_elementsy(1,2,3,4)=1.0_dp
  end if
  CALL MPI_WIN_FENCE(0, win, ierr)

  print *,"======================================="
  print *,"my_rank=",my_rank,matrix_elementsy(1,2),matrix_elementsy(1,5), windowsize
  !print *,"my_rank=",my_rank,matrix_elementsy(1,2,3,4),matrix_elementsy(1,2,3,5), windowsize
  print *,"!======================================="
  if (hostrank == 0) then
    WRITE(rowfmt,'(A,I4,A)') '(',testsize,'(1X,F7.2))'
    OPEN(UNIT=12, FILE="aoutput.txt", ACTION="write", STATUS="replace")
    !OPEN(UNIT=12, FILE="aoutput.txt", ACTION="write", STATUS="replace", &
    !   RECL=(7*testsize+10))
    DO i=1,testsize
      WRITE(12,FMT=rowfmt) (matrix_elementsy(i,j), j=1,testsize)
    END DO
  CLOSE(UNIT=12)
  end if
  !!! end sample code

  call MPI_WIN_FENCE(0, win, ierr)
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  call MPI_Win_free(win,ierr)
  call MPI_FINALIZE(IERR)

  end program
