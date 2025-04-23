module module_read_NSSL_refmosaic
!
!   PRGMMR: Ming Hu          ORG: GSL        DATE: 2022-01-20
!
! ABSTRACT: 
!     This routine read in NSSL reflectivity mosaic fields  
!     from different resources:
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind

  implicit none

  public :: read_nsslref
!
!-------------------------------------------------------------------------

! set default to private
  private
  type :: read_nsslref
     integer           ::   mscNlon   ! number of longitude of mosaic data
     integer           ::   mscNlat   ! number of latitude of mosaic data
     integer           ::   mscNlev   ! number of vertical levels of mosaic data
     real, allocatable :: mscValue3d(:,:,:)    ! reflectivity

     real              :: lonMin,latMin,lonMax,latMax
     real*8            :: dlon,dlat
     integer           :: maxlvl

     integer           :: ilevel
     integer,allocatable :: levelheight(:)

     real     ::  rthresh_ref,rthresh_miss 

     character(len=256) ::   mosaicfile
     logical            :: if_fileexist=.false.
     integer            :: var_scale
    contains
      procedure :: init
      procedure :: readtile
      procedure :: close
  end type read_nsslref
!
! constants
!
contains

  subroutine init(this,mypelocal,datapath)
!                .      .    .                                       .
! subprogram:    init
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!         
!   output argument list:
!        
!
    implicit none

    class(read_nsslref) :: this
    integer,intent(in)  :: mypelocal
    character(len=180),intent(in) :: dataPath

    integer :: k,n,nlevel
    character*256   filenameall(200)
!
!**********************************************************************
!

    this%maxlvl = 33
    this%rthresh_ref=-90.0
    this%rthresh_miss=-900.0

    allocate(this%levelheight(this%maxlvl))
    this%levelheight(1)=500
    do k=2,11 
      this%levelheight(k)=this%levelheight(k-1)+250
    enddo
    do k=12,23 
      this%levelheight(k)=this%levelheight(k-1)+500
    enddo
    do k=24,this%maxlvl 
      this%levelheight(k)=this%levelheight(k-1)+1000
    enddo
!    write(*,*) 'set mosaic level=',this%levelheight

    this%mosaicfile='none'
       open(10,file='filelist_mrms',form='formatted',err=300)
       do n=1,200
          read(10,'(a)',err=200,end=400) filenameall(n)
       enddo
300    write(6,*) 'read_grib2 open filelist_mrms failed ',mypelocal
       stop 555
200    write(6,*) 'read_grib2 read msmr file failed ',n,mypelocal
       stop 555
400    nlevel=n-1
       close(10)

       if(nlevel .gt. this%maxlvl) then
          write(*,*) 'vertical level is too large:',nlevel,this%maxlvl
          stop 666
       endif
       if(mypeLocal <= this%maxlvl) then
          this%mosaicfile=trim(filenameall(mypeLocal))
          write(*,*) 'process level:',mypeLocal,trim(this%mosaicfile)
       endif

  end subroutine init

  subroutine close(this)
!                .      .    .                                       .
! subprogram:    init
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!         
!   output argument list:
!        
!
    implicit none

    class(read_nsslref) :: this

    this%mosaicfile='none'
    this%maxlvl=0
    if(allocated(this%levelheight)) deallocate(this%levelheight)
    if(allocated(this%mscValue3d)) deallocate(this%mscValue3d)

  end subroutine close
!

  subroutine readtile(this,mypelocal)
!                .      .    .                                       .
! subprogram:    init
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!         
!   output argument list:
!        
!
    implicit none
!
    include 'netcdf.inc'

    class(read_nsslref) :: this
    integer, intent(in) :: mypelocal
!
    logical  :: fileexist
    integer  :: var_scale

    integer  :: NCID
    integer  :: ntot, ntot2d, mt
    integer  :: nx,ny,nz
    integer  :: yr, mo, da, hr, mn, sc
    real*8   :: rdx,rdy
    real*4   :: rdx4,rdy4
    real     :: rlatmax,rlonmin

    integer ::   mscNlon   ! number of longitude of mosaic data
    integer ::   mscNlat   ! number of latitude of mosaic data
    integer ::   mscNlev   ! number of vertical levels of mosaic data
    real, allocatable :: msclon(:)        ! longitude of mosaic data
    real, allocatable :: msclat(:)        ! latitude of mosaic data
    real, allocatable :: msclev(:)        ! level of mosaic data
    real, allocatable :: mscValue(:,:)    ! reflectivity
    integer  :: height
    integer  :: worklevel
    integer  :: status
    
    real   :: lonMin,latMin,lonMax,latMax
    real*8 :: dlon,dlat

    integer  :: i,j,k
    integer*2, dimension(:),   allocatable  :: var

    character*256   mosaicfile 
!
!   deal with certain tile
!
    mosaicfile=trim(this%mosaicfile)
    fileexist=.false.
   
    inquire(file=trim(mosaicfile),exist=fileexist)
    if(mypeLocal > this%maxlvl) fileexist=.false.

    if(.not.fileexist) then
       this%if_fileexist=.false.
       return
    else
       this%if_fileexist=.true.
    endif

         call read_grib2_head(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy)
         var_scale=1
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)

    if(mypelocal==1) then
       write(*,*) 'mscNlon,mscNlat,mscNlev'
       write(*,*) mscNlon,mscNlat,mscNlev
       write(*,*) 'dlon,dlat,lonMin,lonMax,latMax,latMin'
       write(*,*) dlon,dlat,lonMin,lonMax,latMax,latMin
    endif

    this%var_scale=var_scale
    this%mscNlon=mscNlon
    this%mscNlat=mscNlat
    this%mscNlev=mscNlev

    allocate(msclon(this%mscNlon))
    allocate(msclat(this%mscNlat))
    allocate(msclev(this%mscNlev))

    do i=1,mscNlon
       msclon(i)=lonMin+(i-1)*dlon
    enddo
    do i=1,mscNlat
       msclat(i)=latMin+(i-1)*dlat
    enddo
    deallocate(msclon)
    deallocate(msclat)
    deallocate(msclev)
!
!  ingest mosaic file and interpolation
!  
    mscNlev=1
    allocate(this%mscValue3d(mscNlon,mscNlat,mscNlev))
    allocate(mscValue(mscNlon,mscNlat))

       ntot = nx*ny*nz
       call read_grib2_sngle(mosaicfile,ntot,height,mscValue)
       if(this%levelheight(mypeLocal) .eq. height) then
            worklevel=mypeLocal
       else
            worklevel=0
            do k=1,this%maxlvl
               if (this%levelheight(k) .eq. height) worklevel=k
            enddo
            if(worklevel==0) then
               write(6,*) 'Error, cannot find working level', &
                         mypeLocal,this%levelheight(mypeLocal), height
               stop 12345
            else
               write(6,*) 'Find new level for core ',mypeLocal,' new level is',worklevel   
            endif
       endif
       this%ilevel=worklevel
       this%mscValue3d(:,:,1)=mscValue(:,:)
       write(*,*) 'level max min height',mypeLocal,maxval(this%mscValue3d),minval(this%mscValue3d),height

    deallocate(mscValue)

    this%lonMin=lonMin
    this%latMin=latMin
    this%lonMax=lonMax
    this%latMax=latMax
    this%dlon=dlon
    this%dlat=dlat

  end subroutine readtile

end module module_read_NSSL_refmosaic
