!-----------------------------------------------------------------------------------------
!    program d7
!  
!    developed     in :  12/04/00
!    modified      in :  24/03/00 
!    last modification:  07/07/00 
!                      
!    contents         :  subroutine with exergy loss profile and
!                        quasi-reversible profile with feed correction
!
!    considered       :  molar enthalpy of dist2k and Hysim.
!                        in dist2k and Hysim - h,s and ex are molar
!                        in hysim_column_read, they were calculed for s,h and ex absolut.
!                        this new version considered than exit various types de arranjos to 
!                        multicomponent system
!
!    variables :
!    dist               - propriedades do destilado
!    bott               - propriedades do fundo
!    feed               - propriedades do alimentação
!    stage              - propriedades do estágio
!    revstage           - propriedades do estágio reversível
!    data_type          - propriedades tipo
!    dlinha             - propriedades do destilado
!    i,j,ii,jj          - indices
!
!-----------------------------------------------------------------------------------------
    
    
    use type_definitions
    use array_sizes

    implicit none

!   variables
!   ---------
    
    type (stream) :: dist, bott, feed,side_q
    type (column_parameters) :: col_params
    type (column_stage), dimension(nmax_stages) :: stage
    type (column_revstage), dimension(nmax_stages) :: revstage

    integer,dimension(nmax_lks) :: lks
    integer,dimension(nmax_hks) :: hks

    integer :: num_lks, num_hks,ii,lk,hk
    integer :: istage, i,num_comps

    !character*20 :: cmnam, aux_char
    character*20  :: sys_file       ! Type of system          (20)
    character*15  :: in_file        ! elp and prp  subroutine (21)
    character*15  :: dex_file       ! elp subroutine          (23)
    character*15  :: rev_file       ! prp subroutine          (22)
    character*15  :: gnu_file       ! d7 program              (24)
    character*8   :: date,data_type    
    !character (len=100) :: directory
    !character (len=100), external:: curdir@
    ! character*60 line
  
    real :: cgcc_hf,sum

    !double precision :: ex_perdida
    double precision :: ex_utilities 
    double precision :: ex_streams
    double precision :: ex_lostt
    double precision :: col_efi
    double precision :: media
    double precision :: ex_rebo
    double precision :: ex_cond
    double precision :: des_p
    real, dimension(nmax_stages) :: exlost
    double precision ::  b, c
  
   
    

!   call subroutines
!   ----------------
    
    call  get_file5 (in_file,sys_file, rev_file,dex_file,gnu_file)
    
    


!   valores de h, s e ex voltam em J e J/K por tempo (nao e' molar!!!!)
!   -------------------------------------------------------------------

    call hysim_column_read (in_file,num_comps, col_params, feed,         &
                            dist, bott, stage,data_type,side_q)
    
    call elp  (in_file, num_comps, col_params, feed,                     &
                            dist, bott, stage, data_type,side_q,         &
                            ex_utilities,ex_streams, col_efi, &
                            media,b,c,ex_cond, ex_rebo,ex_lostt,des_p,sum)

    call prp  (in_file, num_comps, col_params, feed,                     &
               dist, bott, stage,revstage,side_q,cgcc_hf,sys_file)

    
  
!   writing on desktop 
!   ------------------

     write (*, '(/a)')    'Exergy Loss Profile And CGCC With Feed Correction  (C) Luciana Saliba 2000 '
     write (*, '(a,/)') '-----------------------------------------------------------------------------'
     calldate_and_time(date)
     write (*, *) '# gerado por d7.exe , em ', date(7:8) // '/' // date(5:6) // '/' // date(1:4)  



!   write the result - (file: rev_file)
!   ------------------------------------

     open (unit=22, file= rev_file, status= 'unknown')

       do istage = 1,col_params%num_stages    
        if ((num_comps .eq. 2)) then
                write (22,'(i4, 3f10.3,5f17.3)') istage, stage(istage)%t, revstage(istage)%lm,  &
                                         revstage(istage)%vm,revstage(istage)%h_def, &
                                         revstage(istage)%cgcc_h,stage(istage)%alfha, &
                                         revstage(istage)%hlm,revstage(istage)%hvm
                    if ((istage .gt. 1) .and. (istage .lt. col_params%num_stages)) then
                        if (( feed%t .lt. stage(istage+1)%t) .and. ( feed%t .gt. stage(istage)%t))then    
                            write (22,'(a4,3f10.3,5f17.3)') 'fs', feed%t,feed%lm, feed%vm, &
                                                    feed%h_def, cgcc_hf,stage(istage)%alfha, &
                                                   feed%hlm, feed%hvm
                        end if

                    end if
              else         
         !if (feed%x(lk) .eq. feed%y(lk)) then (para liquido subresf e vapor super aquescido)
                write (22,'(i4, 3f10.3,5f17.3)') istage, stage(istage)%t, revstage(istage)%lm,  &
                                         revstage(istage)%vm,revstage(istage)%h_def, &
                                         revstage(istage)%cgcc_h,stage(istage)%alfha, &
                                         revstage(istage)%hlm,revstage(istage)%hvm   
                     
         
          end if
        end do
              
     close (22)
       

!   write  exergy loss profile - (file = dex_file)
!   ----------------------------------------------

    open (unit = 23, file = dex_file, status = 'unknown')
    
    stage(1)%exlost = 0.0
    stage(col_params%num_stages)%exlost = 0.0
    do i = 1, col_params%num_stages
        write (23,'(i4, 4f16.2)') i, stage(i)%t, stage(i)%exlost,stage(i)%ex_qint  !, exlost(i)
    end do
    write (23,*) 'ex_cond = ', ex_cond
    write (23,*) 'ex_rebo = ', ex_rebo
    write (23,*) 'bott%ex=', bott%ex
    write (23,*) 'dist%ex=', dist%ex
    write (23,*) 'feed%ex=', feed%ex
    write (23,*) 'ex_utilities =', ex_utilities
    write (23,*) 'ex_streams=', ex_streams 
    write (23,*) 'ex_lostt=' , ex_lostt
    write (23,*) 'col_efi=' , col_efi
    write (23,*) 'media=', media 
    write (23,*) 'des_p = ', des_p
  
    
    call date_and_time(date)
    write (23, *) '# gerado por d7.exe , em ', date(7:8) // '/' // date(5:6) // '/' // date(1:4)  
    close (23)
    !ilen = len_trim (dex_file)
    !write (*,*) ' - Exergy Loss Profile date:    ', dex_file(1:ilen) 
    
    


!   write profiles in grafic file - (file = gnu_file)
!   --------------------------------------------------

    open (unit = 24, file = gnu_file, status = 'unknown')

    write (24,*)  'reset'
    write (24,*)  'set function style lines'
    write (24,*)  'set size 1.0,1.0'
    !write (24,*) 'set time 0.0,0.0'
    write (24,*)  'set multiplot'
    write (24,*)  'set size 0.5,0.5'
    write (24,*)  'set origin 0.0,0.5'
    write (24,*)  'set grid'
    write (24,*)  'set nokey'
    write (*,*)   'set fontname''lucida console''10'''
    !write (24,*)  'set title'' Perfil de Exergia Perdida ('// dex_file // ')'''
    write (24,*)  'set xlabel'' Ex_p/(kJ/h)'''
    write (24,*)  'set ylabel ''T/K'''
    !write (24,*)  'set yrange [] reverse'
    write (*,*)   'set fontname''lucida console''10''' 
    write (24,*)  'plot ''' // dex_file // ''' using 3:2 with lines'   
    write (24,*)  'set size 0.5,0.5'
    write (24,*)  'set origin 0.5,0.5'
    write (24,*)  'set grid'
    write (24,*)  'set nokey'
    !write (24,*)  'set title''Perfil Pseudo-Reversível ('// rev_file // ')'''
    write (24,*)  'set xlabel''H/(mJ/h)'''
    write (24,*)  'set ylabel ''T/K'''
    !write (24,*)  'set yrange [] reverse' 
    write (*,*)   'set fontname''lucida console''10'''
    write (24,*)  'plot ''' // rev_file // ''' using 6:2 with lines'   
    write (24,*)  'set size 0.5,0.5'
    write (24,*)  'set origin 0.0,0.0'
    write (24,*)  'set grid'
    write (24,*)  'set nokey'   
    write (*,*)   'set fontname''lucida console''10'''
    !write (24,*)  'set title''Perfil de Exergia Perdida('// dex_file // ')'''
    write (24,*)  'set xlabel''Ex_p/(kJ/h)'''
    write (24,*)  'set ylabel ''estágios[-]'''
    write (24,*)  'plot ''' // dex_file // ''' using 3:1 with lines'   
    write (24,*)  'set size 0.5,0.5'
    write (24,*)  'set origin 0.5,0.0'
    write (24,*)  'set grid'
    write (24,*)  'set nokey'
    write (*,*)   'set fontname''lucida console''10'''
    !write (24,*)  'set title''Perfil Pseudo-Reversível ('// rev_file // ')'''
    write (24,*)  'set xlabel''H/(mJ/h)'''
    write (24,*)  'set ylabel ''estágios[-]''' 
    write (24,*)  'plot ''' // rev_file // ''' using 6:1 with lines'   
    write (24,*)  'set nomultiplot'
    close (24)

    

 end
!----------------------------------------------------------------------------------------    
! Subroutine      -    Exergy Loss Profile (exloss)
! Function        -    Computes exergy loss profile
!
!
! Date        :: 12/04/00
! Last modify :: 03/10/00
!
!
!       variables:
!       ex_utilities       - exergia gasta
!       ex_streams         - exergia necessaria
!       ex_lostt           - exergia total perdida na coluna
!       
!       col_efi            - coeficiente de eficiência da coluna
!       media              - media de perdas na coluna
!       sum                - calculo de somatorio
!       soma               - calculo de somatorio
!       ex_rebo            - exergia do refervedor
!       ex_cond            - exergia do condensador
!       ex_rect_loss       - exergia perdida na retificação
!       ex_strip_loss      - exergia perdida na exaustão
!       ex_rev             - exergia reversível
!       q_heater           - 
!       ex_heater          -
!       des_pp             - desvio padrão de exergia
!
!-----------------------------------------------------------------------------------------
    subroutine elp (in_file, num_comps, col_params, feed,             &
                    dist, bott, stage, data_type,side_q,ex_utilities, &
                    ex_streams, col_efi,media,b,c,ex_cond,ex_rebo,    &
                    ex_lostt,des_p,sum)

    use type_definitions
    use array_sizes

    implicit none


!   variables
!   ---------

    type (stream) :: dist, bott, feed,side_q
    type (column_parameters) :: col_params
    type (column_stage), dimension(nmax_stages) :: stage
    type (column_revstage), dimension(nmax_stages) :: revstage
   
    integer :: num_comps 
    integer :: istage
    integer :: i
    integer :: nst
    !integer :: ilen
    !integer :: ifeed
    
    character*15   :: in_file
    !character*8   :: date
    character*8    :: data_type
    !character*15  :: dex_file
    !character*15  :: gnu_file
    !character (len=100) :: directory
    !character (len=100), external:: curdir@
  
    

    double precision :: b,c

    real, dimension(nmax_stages) :: exlost
    real, parameter  :: t0 = 298.15
    double precision :: des_p
    double precision :: ex_streams
    double precision :: ex_utilities
    double precision :: ex_lostt
    double precision :: media
    real :: sum
    double precision:: t1, t2
    double precision:: ex_rebo
    double precision:: ex_cond
    double precision:: t_logmean 
    double precision :: col_efi     
    double precision :: tlm       
    !real :: ex_rev
    !real :: q_heater,ex_heater
    !real :: ex_feedl
    !real :: ex_feedv
    

!   call subroutine
!   ----------------    
    
    call hysim_column_read (in_file, num_comps, col_params, feed,       &
                            dist, bott, stage, data_type,side_q)

    
    nst = col_params%num_stages

      



!   feed calculation
!   ---------------------
    feed%cond = feed%liq / feed%flow
             
    
!   condenser and reboiler stage exergy
!   -----------------------------------
    
    t1 = stage(1)%t
    t2 = stage(2)%t
    tlm = t_logmean (t1, t2)
    ex_cond = (1 - t0 / tlm) * col_params%qcond

    t1 = stage(nst)%t
    t2 = stage(nst-1)%t
    tlm = t_logmean (t1, t2) 
    ex_rebo = (1 - t0 / tlm) * col_params%qrebo



!   condenser and reboiler side
!   ---------------------------     
    do i = 2,nst-1      
        stage(i)%qint = stage(i)%q 
        write(*,*) stage(i)%qint
        stage(i)%ex_qint = ((1 - t0/stage(i)%t) * stage(i)%qint)
    end do

  
   
!   stage by stage
!   --------------
    do i = 1, nst      
        stage(i)%exl = (stage(i)%hl - t0 * stage(i)%sl)
        stage(i)%exv = (stage(i)%hv - t0 * stage(i)%sv)
    end do    
    
    do i = 2, nst - 1
       stage(i)%exlost = (stage(i-1)%exl + stage(i+1)%exv - stage(i)%exl - stage(i)%exv)&
                         + stage(i)%ex_qint
      
       exlost(i) = stage(i)%exlost
    end do



!   feed streams
!   ------------
!    do ifeed = 1, col_params%num_feeds  ! ONE FEED ONLY
        istage = feed%stage
        
        ! 15/03/2000: working for sl, flash, sv
	    ! -------------------------------------


        if (feed%liq .gt. 0.0 .and. feed%vap .eq. 0.0) feed%phase = 'l'
        if (feed%liq .eq. 0.0 .and. feed%vap .gt. 0.0) feed%phase = 'v'
        if (feed%liq .gt. 0.0 .and. feed%vap .gt. 0.0) feed%phase = 'f'


!   if liquid feed, do exergy balance at feed stage
!   -----------------------------------------------
	    if (feed%phase .eq. 'l' .or. feed%phase .eq. 'c') then 
            stage(istage)%exlost = (stage(istage)%exlost + feed%exl)  !* feed%liq
	    end if	
        !q_heater= 0.0
        !ex_heater= 0.0
        


!   if vapour feed, do liquid exergy balance at proper stage:
!       if dist2k, at stage above feed
!       if hysim, at feed stage
!   ---------------------------------------------------------
	    if (feed%phase .eq. 'v' .or. feed%phase .eq. 'v') then 
            if (data_type .eq. 'dist2k') then
                stage(istage - 1)%exlost = (stage(istage - 1)%exlost + feed%exv)    !* feed%vap
            else
                stage(istage)%exlost = (stage(istage)%exlost + feed%exv)           !* feed%vap
            end if
	    end if	



!   if flashed feed, do exergy balance at proper stage:
!       liquid, at feed stage
!       vapour:
!           if dist2k, at stage above feed
!           if hysim, at feed stage
!   ---------------------------------------------------------
	    if (feed%phase .eq. 'f') then    
            if (data_type .eq. 'dist2k') then
                stage(istage - 1)%exlost = (stage(istage - 1)%exlost + feed%exv)    !* feed%vap
            else
                stage(istage)%exlost = (stage(istage)%exlost + feed%exv)           !* feed%vap
            end if  
          
            stage(istage)%exlost = (stage(istage)%exlost + feed%exl)            !* feed%liq
    	end if	


!    end do


!    do ifeed = 1, col_params%num_side_l
!        istage = side_l(ifeed)%stage
!        stage(istage)%exlost = stage(istage)%exlost - side_l(ifeed)%exl
!    end do

!    do ifeed = 1, col_params%num_side_v
!        istage = side_v(ifeed)%stage
!        stage(istage)%exlost = stage(istage)%exlost - side_v(ifeed)%exv
!    end do


!   condenser / reboiler
!   --------------------
    stage(1)%exlost = stage(2)%exv - stage(1)%exl - dist%ex + ex_cond
    stage(nst)%exlost = stage(nst - 1)%exl + ex_rebo - stage(nst)%exv - bott%ex 



 
!   ex_utilities calculation (ex gasta,que o ambiente fornece ao processo de separação)
!   ------------------------------------------------------------------------------------! +,-
                                                                                        ! +,+
    do i = 1,nst
      ex_utilities = ex_rebo + ex_cond
        if (i .lt. feed%stage) then
           ex_utilities = ex_rebo + ex_cond - stage(i)%ex_qint 
         else if ( i .ge. feed%stage) then
           ex_utilities = ex_rebo + ex_cond + stage(i)%ex_qint 
        end if
    end do


!   ex_streams calculation (ex necessária da coluna)
!   ---------------------------------------------------------
    ex_streams = dist%ex + bott%ex - feed%ex 

   
!   ex_global calculation ( é a exergia perdida na coluna = somatorio das ex_lost no estágios)
!   ------------------------------------------------------------------------------------------
    
    ex_lostt = ex_utilities - ex_streams


!   eficience
!   ----------

    col_efi = (ex_streams/(ex_streams + ex_lostt))
    

!   des_pp calculation
!   --------------------------    
    
    media = 0.0
    stage(1)%exlost = 0.0
    stage(col_params%num_stages)%exlost = 0.0

    do i = 1,nst
        media = media + stage(i)%exlost
    end do

    media = media/nst  
    b = nst-1
    c = 1/b
    
    sum = 0.0
    do i = 1,nst
        sum = sum + (stage(i)%exlost - media)**2
    end do
    write (*,*) sum
    des_p = sqrt(c * sum)


    !do i = 1,nst  
    !    stage(i)%des_pp = sqrt (c * ((stage(i)%exlost - media)**2)) 
    !    des_p = (sqrt (c *  ((ex_streams - media)**2)))  
    !end do 

   
    write (*,*) 'ex_utilities=', ex_utilities
    write (*,*) 'ex_streams=', ex_streams 
    write (*,*) 'media=', media
    write (*,*) 'c=', c
    write (*,*) 'feed%ex=', feed%ex
    write (*,*) 'ex_lostt=' , ex_lostt
    write (*,*) 'col_efi=' , col_efi
    write (*,*) 'des_p=', des_p
    write (*,*) 'tlm =', tlm

 end
!---------------------------------------------------------------------------------------
!   subroutine
!   name         :  CGCC with feed correction
!   developed in : 14/03/00
!   contents     : cgcc calcutate with feed correction
!   modifided    : 14/04/00 and 03/10/00
!
!
!   variables:
!   dist                        - propriedades do destilado
!   bott                        - propriedades do fundo
!   feed                        - propriedades do alimentação
!   side_v                      - propriedades do retirada lateral de vapor
!   side_l                      - propriedades do retirada lateral de liquido
!   dlinha                      - propriedades do destilado
!   side_q                      - propriedades do de trocadores laterais de calor
!   dlinha_flow_old             - propriedades do destilado
!   colunm_h                    - entalpia da coluna
!   qrr                         - 
!   qcr                         -
!   cgcc_hf                     - entalpia deficit da alimentação
!
!
!   
!----------------------------------------------------------------------------------------

    subroutine prp (in_file, num_comps, col_params, feed,       &
                    dist, bott, stage,revstage,side_q,cgcc_hf,sys_file)

    use type_definitions
    use array_sizes


    implicit none

 
!   variables 
!   ----------

    type (stream) :: dist, bott, feed , side_v, side_l, dlinha,side_q
    type (column_parameters) :: col_params
    type (column_stage), dimension(nmax_stages) :: stage 
    type (column_revstage), dimension(nmax_stages) :: revstage
    
    integer,dimension(nmax_lks) :: lks
    integer,dimension(nmax_hks) :: hks
    integer :: num_comps, num_lks,num_hks
    integer :: num_qint
    integer :: ii,i, istage
    integer :: summ
    integer :: lk , hk
    real :: dlinha_flow_old
    real :: colunm_h, qrr,qcr
    real :: cgcc_hf
    
    character*15 :: in _file
    character*15 :: sys_file
    character*8  :: data_type
    character*60 line


    
!   call subroutine -  type system defination
!   --------------------------------------------
    call hysim_column_read (in_file, num_comps, col_params, feed,       &
                            dist, bott, stage,data_type,side_q)



!   lk and  Hk calculation
!   ------------------------     
    i = 1
    istage = 1 
    lk = 1
    hk = lk + 1        

   
	
       
!    heat_load definiton  in column
!    ------------------------------
     do ii = 2, col_params%num_stages - 1
       
            stage(ii)%q = stage(ii)%q
            if (stage(ii)%q .lt. 0.0 ) then
                side_q%ss= ii 
                side_q%qintc  = stage(ii)%q
                side_q%qintr  = 0.0           
                write (*,*) side_q%ss,side_q%qintc,side_q%qintr
              else if (stage(ii)%q .gt. 0.0 )then
                side_q%ss = ii 
                side_q%qintr  = stage(ii)%q
                side_q%qintc  = 0.0        
                write (*,*) side_q%ss,side_q%qintr,side_q%qintc
           end if
     
     end do

!   feed calculation
!   ---------------------
    feed%cond = feed%liq / feed%flow

    
!   side entrance calculation
!   ---------------------------
!   feed%s = feed%sl + feed%sv
!   feed%hs = (feed%hsl * feed%sl) + (feed%hsv * feed%sv)


!   relative volatility calculation
!   -------------------------------
!    do i = 1,col_params%num_stages
!        stage(i)%alfha = (stage(i)%y(lk) * (1 - stage(i)%x(lk) ))/  &
!                   (stage(i)%x(lk) * (1 - stage(i)%y(lk) ))
!    end do

 

!   minimum liquid and vapour flow calculation - start                        
!   --------------------------------------------------
    dlinha = dist
    if(dist%liq .gt. 0.0) then
        dlinha%flow = dist%liq
        write (*,*) dlinha%flow
      else
        dlinha%flow = dist%vap
        !dlinha%flow = stage(1)%l
        write(*,*) dlinha%flow
    end if

    do i = 1, num_comps
        if (dist%liq .gt. 0.0) then
            dlinha%z(i) = dist%x(i)
            dlinha%z(lk) =  dist%x(1) +  dist%x(2)
            dlinha%z(hk) =  dist%x(3)
            write (*,*) dlinha%z(lk),dlinha%z(hk)
          else
            dlinha%z(i) = dist%y(i)
            dlinha%z(lk) =  dist%y(1) + dist%y(2)
            dlinha%z(hk) =   dist%y(3)
            write (*,*) dlinha%z(lk),dlinha%z(hk)
        end if
    end do

    write (*,*) '___________________'
    write (*,*) feed%y(lk)
    write (*,*) feed%y(hk)
    write (*,*) feed%x(lk)
    write (*,*) feed%x(hk)
    write (*,*) '___________________'
    write (*,*) bott%y(lk)
    write (*,*) bott%y(hk)
    write (*,*) bott%x(lk)
    write (*,*) bott%x(hk)
    write (*,*) '___________________'
    


   do i = 1, col_params%num_stages 
    
!   - (only vapour in feed stage)
!   -------------------------------
        if (istage .eq. feed%stage-1) then
         !write (*,*) 'vapor ...'
            if (feed%vap .gt. 0.0) then
               dlinha_flow_old = dlinha%flow  
               dlinha%flow  = dlinha%flow - feed%vap
               dlinha%z(lk) = (dlinha_flow_old * dlinha%z(lk) - feed%vap * feed%y(lk))/ dlinha%flow
               dlinha%z(hk) = (dlinha_flow_old * dlinha%z(hk) - feed%vap * feed%y(hk))/ dlinha%flow
            endif
         end if


!   - ( only liquid in feed stage)
!   --------------------------------
        if (istage .eq. feed%stage) then 
         ! write (*,*) 'liquido ...'  
            if (feed%liq .gt. 0.0) then
                  dlinha_flow_old = dlinha%flow 
                  dlinha%flow  = dlinha%flow - feed%liq
                  dlinha%z(lk) = (dlinha_flow_old * dlinha%z(lk) - feed%liq * feed%x(lk))/ dlinha%flow
                  dlinha%z(hk) = (dlinha_flow_old * dlinha%z(hk) - feed%liq * feed%x(hk))/ dlinha%flow
             end if
        end if

   
!   minimum flow calculation
!   ------------------------
        revstage(i)%lm = dlinha%flow * ( dlinha%z(lk) * stage(i)%y(hk) -  &
                               dlinha%z(hk) * stage(i)%y(lk))/ &
                               ( stage(i)%y(lk) * stage(i)%x(hk) - &
                               stage(i)%y(hk) * stage(i)%x(lk))  
          
        revstage(i)%vm =  dlinha%flow * ( dlinha%z(lk) * stage(i)%x(hk) -  &
                               dlinha%z(hk) * stage(i)%x(lk))/ &
                               ( stage(i)%y(lk) * stage(i)%x(hk) - &
                               stage(i)%y(hk) * stage(i)%x(lk))  
          

!   minimum enthalpy calculation
!   ----------------------------
        if (stage(i)%v .eq.  0.0) then
            revstage(i)%hvm = 0.0
          else
            revstage(i)%hvm = ((revstage(i)%vm * stage(i)%hv)/stage(i)%v)
        end if
        if (stage(i)%l .eq.  0.0) then
            revstage(i)%hlm = 0.0
          else
            revstage(i)%hlm = ((revstage(i)%lm * stage(i)%hl)/stage(i)%l)
        end if


        !revstage(istage)%hlm = ((revstage(istage)%lm * stage(istage)%hl)/stage(istage)%l)
        !if (stage(1)%hv .eq. 0.0) stage(1)%hv = 0.000001
        !revstage(istage)%hvm = ((revstage(istage)%vm * stage(istage)%hv)/stage(istage)%v)
   

    if ((num_comps .eq. 2)   .or. (feed%x(lk) .eq. feed%y(lk)))   then
!   - (virtual  feed stage)
!   ------------------------
            if (feed%x(lk) .eq. feed%y(lk)) then
                feed%lm = 0.0
                feed%vm = 0.0
              else       
                feed%lm = dlinha%flow * ( dlinha%z(lk) * feed%y(hk) -  &
                               dlinha%z(hk) * feed%y(lk))/ &
                               ( feed%y(lk) * feed%x(hk) - &
                               feed%y(hk) * feed%x(lk))  
          
                feed%vm =  dlinha%flow * ( dlinha%z(lk) * feed%x(hk) -  &
                               dlinha%z(hk) * feed%x(lk))/ &
                               ( feed%y(lk) * feed%x(hk) - &
                               feed%y(hk) * feed%x(lk))
           
                feed%hlm =  feed%lm * feed%hl_m
                feed%hvm =  feed%vm * feed%hv_m
           end if
    

!   feed%h_def calculation
!   ------------------------
            feed%h_def = feed%hlm - feed%hvm + dist%h - feed%h 

            cgcc_hf = (-(stage(1)%q + side_q%qintc  ) + feed%h_def )/1000
    end if

 end do



!   h_def calculation
!   ------------------
     do istage = 1, feed%stage-1     !total condenser
            revstage(istage)%h_def =  revstage(istage)%hlm - revstage(istage)%hvm + dist%h 
            if (istage .eq. feed%stage-1) then
               if (feed%vap .gt. 0.0) then
                   revstage(istage)%h_def = revstage(istage)%hlm - revstage(istage)%hvm + &
                                             dist%h - feed%hv   !* feed%vap    
               end if
            end if  
     end do 

     do istage = feed%stage, col_params%num_stages                    
       revstage(istage)%h_def = revstage(istage)%hlm  - revstage(istage)%hvm + dist%h - feed%h           
     end do


!   cgcc_h  calculation
!   --------------------   
     do istage = 1,col_params%num_stages            
        revstage(istage)%cgcc_h = (- stage(1)%q + revstage(istage)%h_def)/1000
              if ( istage .ge. side_q%ss)  then        
                   revstage(istage)%cgcc_h = (- (stage(1)%q + side_q%qintc +side_q%qintr)  &
                                        + revstage(istage)%h_def)/1000
               
              end if
     end do
close (20)


!   colunm_h calculation
!   ---------------------
    colunm_h = dist%h + bott%h - feed%h
    qcr = revstage(feed%stage)%h_def
    qrr = colunm_h  + qcr 
    write (*,*)  colunm_h,qrr, qcr


 end
! -----------------------------------------------------------------------------------------------
!
!   subroutine hysim_column_read
!
!   reads column data from file (*.hyc-HYSIM)
!                                       
!   to do:
!   - read more than 2 products
!   - read more than 1 feed
!                                
! -----------------------------------------------------------------------------------------------

    subroutine hysim_column_read (in_file, num_comps, col_params, feed,       &
                                  dist, bott, stage,data_type, side_q)


    use array_sizes
    use type_definitions

    implicit none

    type (stream) :: dist, bott, feed,side_q
    type (column_parameters) :: col_params
    type (column_stage), dimension(nmax_stages) :: stage
   
   

    integer num_comps, i, ii, jj
    real :: version, temp_offset

    real, parameter :: t0 = 298.15

    character*60 line
    character*15 in_file
    character*32 name
    character*8 data_type

    integer :: istage

    open (unit=21, file=in_file, status='old')


!   read component data
!   -------------------


    read (21,'(a)') line
    read (21,'(1x,a)') line
    if (line .eq. 'dist2k') then
        data_type = 'dist2k'
    else
        data_type = 'hysim'
    end if

    write (*,*) data_type

    read(21,'(a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,i2,a5,i5)') ii, line, jj
    read (21,'(1x,a)') line 
    read (21,'(1x,a)') line
    
    read (21,'(2x,i5,a)') num_comps, line
    read (21,'(2x,i5,a)') col_params%num_stages, line
    read (21,'(2x,i5,a)') col_params%num_feeds, line
    read (21,'(2x,i5,a)') ii, line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,i1)') ii
    read (21,'(1x,i1)') ii
    read (21,'(1x,a)') line
    read (21,'(1x,i1)') ii
    read (21,'(1x,i1)') ii
    read (21,'(1x,a)') line
    read (21,'(1x,i1)') ii
    read (21,'(1x,i1)') ii
    read (21,'(1x,a)') line

    do i= 1, num_comps
       read(21,'(1x,a15)') name
    end do

    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,f15.5,a)') version, line
    read (21,'(1x,i15,a)') version, line
    read (21,'(1x,f15.5,a)') version, line
    read (21,'(1x,i15,a)') version, line
    read (21,'(1x,i15,a)') version, line
    read (21,'(1x,f15.5,a)') version, line
    read (21,'(1x,i15,a)') temp_offset, line



!   carga térmica dos trocadores
!   ----------------------------

    read (21,'(1x,a)') line
    read (21,'(1x,a)') line

    do istage = 1, col_params%num_stages
       read (21,'(1x,i15,f15.5)') ii, stage(istage)%q
       if(istage .eq. 1) then
          col_params%qcond= stage(istage)%q
        else if (istage .eq. col_params%num_stages) then
          col_params%qrebo= stage(istage)%q
       end if
    end do
    
      



!   read feed information
!   ---------------------

    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21,'(1x,a)') line
    read (21, *) feed%stage, feed%t, feed%p, feed%liq, feed%vap
    read (21, '(1x,a)') line
    read (21,'(4x,i12,4f16.6)') ii, feed%hl_m, feed%hv_m, feed%sl, feed%sv
    read (21,'(1x,a)') line
    read (21,'(4x,a)') line
    read (21,'(1x,a)') line
    do i= 1, num_comps
        read (21,'(7x,i10,2f16.6)') ii, feed%x(i), feed%y(i)
    end do

    feed%flow = feed%liq + feed%vap
    feed%hl = feed%hl_m * feed%liq
    feed%hv = feed%hv_m * feed%vap
    feed%sl = feed%sl * feed%liq
    feed%sv = feed%sv * feed%vap

    feed%exl = feed%hl - (t0 * feed%sl)
    feed%exv = feed%hv - (t0 * feed%sv)
 
    feed%h  = feed%hl + feed%hv
    feed%s  = feed%sl + feed%sv
    feed%ex = feed%exl + feed%exv

    do i = 1, num_comps
        feed%z(i) = (feed%x(i) * feed%liq + feed%y(i) * feed%vap) / feed%flow
    end do


!   read product information
!   ---------------------

    read (21, '(1x,a)') line
    read (21, '(1x,a)') line
    read (21, '(1x,a)') line
    read (21, '(1x,a)') line
    read (21, '(1x,a)') line
    read (21, *) dist%stage, dist%t, dist%p, dist%liq, dist%vap
    read (21, *) bott%stage, bott%t, bott%p, bott%liq, bott%vap
    read (21, '(1x,a)') line
    read (21, *) dist%stage, dist%hl_m, dist%hv_m, dist%sl, dist%sv
    read (21, *) bott%stage, bott%hl_m, bott%hv_m, bott%sl, bott%sv
    read (21, '(1x,a)') line
    read (21, '(4x,a)') line
    read (21, '(1x,a)') line
    do i= 1, num_comps
        read (21,'(6x,i11,2f16.6)') ii, dist%x(i), dist%y(i)
    end do
    read (21, '(4x,a)') line
    read (21, '(1x,a)') line
    do i= 1, num_comps
        read (21,'(6x,i11,2f16.6)') ii, bott%x(i), bott%y(i)
    end do

    dist%flow = dist%liq + dist%vap
    dist%hl = dist%hl_m * dist%liq
    dist%hv = dist%hv_m * dist%vap
    dist%sl = dist%sl * dist%liq
    dist%sv = dist%sv * dist%vap

    dist%exl = dist%hl - t0 * dist%sl 
    dist%exv = dist%hv - t0 * dist%sv 

    dist%h  = dist%hl + dist%hv
    dist%s  = dist%sl + dist%sv
    dist%ex = dist%exl + dist%exv

    do i = 1, num_comps
        dist%z(i) = (dist%x(i) * dist%liq + dist%y(i) * dist%vap) / dist%flow
    end do



    bott%flow = bott%liq + bott%vap

    bott%hl = bott%hl_m * bott%liq
    bott%hv = bott%hv_m * bott%vap
    bott%sl = bott%sl * bott%liq
    bott%sv = bott%sv * bott%vap

    bott%exl = bott%hl - t0 * bott%sl
    bott%exv = bott%hv - t0 * bott%sv
    bott%h   = bott%hl + bott%hv
    bott%s   = bott%sl + bott%sv
    bott%ex  = bott%exl + bott%exv

    do i = 1, num_comps
        bott%z(i) = (bott%x(i) * bott%liq + bott%y(i) * bott%vap) / bott%flow
    end do


!   read stage by stage information
!   -------------------------------

    read(21,'(1x,a)')line
    read(21,'(1x,a)')line

    do i= 1, col_params%num_stages
         read(21,'(4x,i13,4f16.6)') istage, stage(i)%t, stage(i)%p, stage(i)%l, stage(i)%v
    end do

    read(21,'(1x,a)')line

    do i= 1, col_params%num_stages
        read(21,'(4x,i12,4f16.6)') istage, stage(i)%hl, stage(i)%hv, stage(i)%sl, stage(i)%sv

        stage(i)%hl = stage(i)%hl * stage(i)%l
        stage(i)%hv = stage(i)%hv * stage(i)%v
        stage(i)%sl = stage(i)%sl * stage(i)%l
        stage(i)%sv = stage(i)%sv * stage(i)%v

        stage(i)%exl = stage(i)%hl - t0 * stage(i)%sl
        stage(i)%exv = stage(i)%hv - t0 * stage(i)%sv
    end do

    read(21,'(1x,a44)') line

    do istage= 1, col_params%num_stages
        read(21,'(2x,i4,a12)') ii, line
        read(21,'(1x,a)') line
        do i= 1, num_comps
            read (21,'(6x,i11,2f16.10)') ii, stage(istage)%x(i), stage(istage)%y(i)
        end do
    end do


    feed%t= feed%t + 273.15 + temp_offset
    dist%t= dist%t + 273.15 + temp_offset
    bott%t= bott%t + 273.15 + temp_offset

    do i= 1, col_params%num_stages
        stage(i)%t= stage(i)%t + 273.15 + temp_offset
    end do

    close (21)
    return
    end

!----------------------------------------------------------------------------------------------
! Subroutine  - Data read
!
! Date : 12/04/00
!
! subroutine get_file5 (in_file,sys_file, rev_file,dex_file,gnu_file)
!----------------------------------------------------------------------------------------------

    subroutine get_file5(in_file,sys_file,rev_file,dex_file,gnu_file)

    character*15 :: cmnam, aux_char
    character*15 :: in_file,sys_file,rev_file,dex_file,gnu_file



    write (*, '(/a)')    'Exergy Loss Profile And CGCC With Feed Correction  (C) Luciana Saliba 2000 '
    write (*, '(a,/)') '-----------------------------------------------------------------------------'


    in_file = cmnam()
    if (in_file .eq. ' ') then
        write (*, '(a\)') 'input file (.hyc):    '
        read (*, '(a)') aux_char
        in_file =   aux_char
    end if

    sys_file = cmnam()
    if (sys_file .eq. ' ') then
        write (*, '(a\)') 'type system (.sys): '
        read (*, '(a)') aux_char
        sys_file = aux_char
    end if

    rev_file = cmnam()
    if (rev_file .eq. ' ') then
        write (*, '(a\)') 'output file with cgcc results (.rev): '
        read (*, '(a)') aux_char
        rev_file = aux_char
    end if

    dex_file = cmnam()
    if (dex_file .eq. ' ') then
        write (*, '(a\)') 'output file with exergy loss profile (.dex): '
        read (*, '(a)') aux_char
        dex_file =  aux_char
    end if

    gnu_file = cmnam()
    if (gnu_file .eq. ' ') then
        write (*, '(a\)') ' grafic output file with the profile (.plt): '
        read (*, '(a)') aux_char
        gnu_file =  aux_char
    end if 
  
   end

! =======================================================================

    double precision function t_logmean (t1, t2)

    implicit none

    double precision :: t1, t2, t_logmean

    if (t1. ne. t2) then
        t_logmean = (t1 - t2) / alog (t1 / t2)
    else
        t_logmean = t1
    end if
    
    end

! =======================================================================

                                                             
