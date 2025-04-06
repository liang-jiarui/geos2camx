C**** WRT_SPEC_TABLE
c
      subroutine wrt_spec_table(num_camx_species,camx_species_names,
     &              num_geos_species,geos_species_names,map_idx,lgas)
      implicit none
c
c-----------------------------------------------------------------------
c
c     This routine prints out a table of species names for each model,
c     indicating which species are used in the species mapping.
c
c   Arguments:
c     Inputs:
c        num_camx_species    I number of CAMx species in mapping table
c        camx_species_names  C CAMx species names
c        num_geos_species    I number of GeosChem species in mapping table
c        geos_species_names  C GeosChem species names
c        map_idx             I species mapping table
c        logical             L .TRUE. if species is gas species
c     Outputs:
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer      num_camx_species
      character*12 camx_species_names(*)
      integer      num_geos_species
      character*12 geos_species_names(*)
      integer      map_idx(500,500)
      logical      lgas(*)
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer icamx, igeos, i
      logical lfound
c
      logical, allocatable :: l_use_camx_spec(:)
      logical, allocatable :: l_use_geos_spec(:)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- allocate and initializze local arrays ---
c
      allocate( l_use_camx_spec(num_camx_species) )
      l_use_camx_spec = .FALSE.
      allocate( l_use_geos_spec(num_geos_species) )
      l_use_geos_spec = .FALSE.
c
c  ---- write out table of species used ---
c
      do icamx=1,num_camx_species
        do i=1,99
          if( map_idx(icamx,i) .GT. 0 ) then
              l_use_camx_spec(icamx) = .TRUE.
              l_use_geos_spec(map_idx(icamx,i)) = .TRUE.
          endif
        enddo
      enddo
c
c --- GeosChem species that are mapped ---
c
      write(*,'(30A1)') ('-',i=1,30)
      write(*,'(A,/,A)') '   GeosChem Species that are ',
     &                   '   mapped to a CAMx Species'
      write(*,'(30A1)') ('-',i=1,30)
      do igeos=1,num_geos_species
         if( l_use_geos_spec(igeos) ) 
     &            write(*,'(10X,A)') geos_species_names(igeos)
      enddo
      write(*,'(30A1)') ('-',i=1,30)
      write(*,'(//)')
c
c --- GeosChem species that are not mapped ---
c
      write(*,'(35A1)') ('-',i=1,35)
      write(*,'(A,/,A)') '   GeosChem Species that will ',
     &                   '   not be mapped to a CAMx Species'
      write(*,'(A)') '   These species will not be used.'
      write(*,'(35A1)') ('-',i=1,35)
      do igeos=1,num_geos_species
         if( .NOT. l_use_geos_spec(igeos) ) 
     &            write(*,'(10X,A)') geos_species_names(igeos)
      enddo
      write(*,'(35A1)') ('-',i=1,35)
      write(*,'(//)')
c
c --- CAMx species that are mapped ---
c
      write(*,'(60A1)') ('-',i=1,60)
      write(*,'(A)') '   CAMx Species that are mapped to a GepsChem Species'
      write(*,'(A)') '        Species Name    Type     Units'
      write(*,'(60A1)') ('-',i=1,60)
      do icamx=1,num_camx_species
         if( l_use_camx_spec(icamx) ) then
            if( lgas(icamx) ) then
              write(*,'(10X,A,3X,A,6X,A)') camx_species_names(icamx),
     &                                                      'Gas','ppm'
            else
              write(*,'(10X,A,3X,A,6X,A)') camx_species_names(icamx),
     &                                                   'PM','ug/m^3'
            endif
         endif
      enddo
      write(*,'(60A1)') ('-',i=1,60)
      write(*,'(//)')
c
c --- CAMx species that are not mapped ---
c
      lfound = .FALSE.
      do icamx=1,num_camx_species
         if( .NOT. l_use_camx_spec(icamx) ) lfound = .TRUE.
      enddo
      if( lfound ) then
         write(*,'(40A1)') ('-',i=1,40)
         write(*,'(A)') '   CAMx Species with no mapping defined.'
         write(*,'(A,/,A)') '   Concentrations for these species will',
     &                   '   be set to zero.'
         write(*,'(40A1)') ('-',i=1,40)
         do icamx=1,num_camx_species
         if( .NOT. l_use_camx_spec(icamx) ) 
     &            write(*,'(10X,A)') camx_species_names(icamx)
         enddo
         write(*,'(40A1)') ('-',i=1,40)
         write(*,'(//)')
      endif
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
