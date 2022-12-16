MODULE construct_path

    USE math
    USE type

    CONTAINS

    SUBROUTINE intersection_path(rayon, sphere1, bool, N, P, t)

        IMPLICIT NONE

        TYPE(ray), INTENT(IN) :: rayon
        TYPE(sphere), INTENT(IN) :: sphere1
        LOGICAL, INTENT(OUT) :: bool
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: N, P
        DOUBLE PRECISION :: t
        DOUBLE PRECISION :: a, b, c, delta, s1, s2

        a=1.
        b=2*DOT_PRODUCT(rayon%direction, rayon%origine-sphere1%origine) 
        c=norm(rayon%origine-sphere1%origine)-sphere1%rayon**2

        delta=b**2-4*a*c

        IF(delta<0)THEN
            bool=.FALSE.
        ELSE
            S1=(-b-(SQRT(delta)))/2*a
            S2=(-b+(SQRT(delta)))/2*a
            IF(S2<0)THEN
                bool=.FALSE.
            ELSE
                IF(S1>0)THEN
                    t=S1
                ELSE
                    t=S2
                END IF

                P=rayon%origine + t*rayon%direction
                N=normalise((P-sphere1%origine))
                bool=.TRUE.
            END IF
        END IF

    END SUBROUTINE intersection_path

    SUBROUTINE intersection_scene_path(rayon, scenes, inter_global, global_N, global_P, sphere_id, t_min)

        IMPLICIT NONE

        TYPE(ray), INTENT(IN) :: rayon
        TYPE(scene), INTENT(IN) :: scenes
        LOGICAL, INTENT(OUT) :: inter_global
        INTEGER, INTENT(OUT) :: sphere_id
        DOUBLE PRECISION, INTENT(OUT) :: t_min
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: global_N, global_P
        DOUBLE PRECISION, DIMENSION(3):: local_N, local_P
        DOUBLE PRECISION :: t
        LOGICAL:: bool
        INTEGER :: i

        inter_global=.FALSE.
        t_min=1d99

        DO i=1,scenes%nb_scene

            CALL intersection_path(rayon, scenes%spheres(i), bool, local_N, local_P, t)
            IF(bool)THEN
                inter_global=.TRUE.
                IF(t<t_min)THEN
                    t_min=t
                    global_N=local_N
                    global_P=local_P
                    sphere_id=i
                END IF
            END IF

        END DO  

    END SUBROUTINE intersection_scene_path

    SUBROUTINE miroir_path(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)

        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT):: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3), INTENT(IN) :: N, P
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(millieu), INTENT(INOUT) :: environement
        TYPE(scene), INTENT(IN) :: scenes
        INTEGER, INTENT(IN) :: nb_rebond_max
        TYPE(ray) :: rayon_miroir

        rayon_miroir%origine=P+0.01*N
        rayon_miroir%direction=reflect(N,rayon%direction)
        CALL get_color_path(rayon_miroir, scenes, intensite_pixel, nb_rebond_max-1,environement, .TRUE.)

    END SUBROUTINE miroir_path

    SUBROUTINE transparent_path(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)

        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3), INTENT(INOUT) :: N, P
        DOUBLE PRECISION, DIMENSION(3) :: N_transparent, tmp
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(millieu), INTENT(INOUT) :: environement
        TYPE(scene), INTENT(IN) :: scenes
        INTEGER, INTENT(IN) :: nb_rebond_max
        TYPE(ray) :: rayon_refracte, new_ray
        DOUBLE PRECISION :: n1, n2, radical, R0, R, ran
        LOGICAL :: entering

        n1=environement%n1
        n2=environement%n2
        N_transparent=N
        entering=.TRUE.

        IF(DOT_PRODUCT(rayon%direction,N)>0)THEN
            n1=environement%n2
            n2=environement%n1
            N_transparent=-N
            entering=.FALSE.
        END IF
        radical=1-(n1/n2)**2*(1-DOT_PRODUCT(N_transparent,rayon%direction)**2)
        IF(radical>0)THEN
            tmp=DOT_PRODUCT(rayon%direction,N_transparent)*N_transparent
            rayon_refracte%origine=P-0.01*N_transparent
            rayon_refracte%direction=(n1/n2)*(rayon%direction-tmp) - N_transparent*SQRT(radical)

            R0=((n1-n2)/(n1+n2))**2
            IF(entering)THEN
                R=R0+(1-R0)*(1+DOT_PRODUCT(rayon%direction,N))**5
            ELSE
                R=R0+(1-R0)*(1-DOT_PRODUCT(rayon_refracte%direction,N))**5
            END IF

            CALL RANDOM_NUMBER(ran)

            IF(ran<R)THEN
                new_ray%origine=P+0.01*N_transparent
                new_ray%direction=reflect(N,rayon%direction)
            ELSE
                new_ray%origine=P-0.01*N_transparent
                new_ray%direction=rayon_refracte%direction
            END IF

        ELSE
            new_ray%origine=P+0.01*N_transparent
            new_ray%direction=reflect(N,rayon%direction)
        END IF

        CALL get_color_path(new_ray, scenes, intensite_pixel, nb_rebond_max-1,environement, .FALSE.)
        
    END SUBROUTINE transparent_path

    SUBROUTINE eclairage_direct_path(intensite_pixel,scenes,P,N, sphere_id,rayon)

        IMPLICIT NONE
        TYPE(scene), INTENT(IN) :: scenes
        TYPE(ray), INTENT(IN) :: rayon
        INTEGER, INTENT(IN) :: sphere_id
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3), INTENT(IN) :: N, P
        DOUBLE PRECISION, DIMENSION(3) :: dir_aleatoir, point_aleatoir, wi, Np, axeOp, N_light, P_light
        LOGICAL :: inter_light
        TYPE(ray) :: rayon_light
        INTEGER :: sphere_id_light
        DOUBLE PRECISION :: d_light2, t_light

        axeOp=normalise(P-scenes%spheres(1)%origine)
        dir_aleatoir=vect_random(axeOp)
        point_aleatoir=dir_aleatoir * scenes%spheres(1)%rayon + scenes%spheres(1)%origine
        wi=normalise(point_aleatoir - P)
        d_light2 = norm(point_aleatoir - P)
        Np=dir_aleatoir

        rayon_light%origine=P+0.01*N
        rayon_light%direction=wi
        CALL intersection_scene_path(rayon_light,scenes,inter_light, N_light, P_light, &
                                sphere_id_light, t_light)
            
        IF(inter_light .AND. t_light**2<d_light2*0.99)THEN
            intensite_pixel=(/0,0,0/)
        ELSE
                intensite_pixel=(scenes%lum%intensite_lumiere/(4*pi*d_light2)*MAX(0.,DOT_PRODUCT(N,wi))*&
                                DOT_PRODUCT(Np,-wi)/DOT_PRODUCT(axeOp,dir_aleatoir))*pi*&
                                ((1-scenes%spheres(sphere_id)%ks)*scenes%spheres(sphere_id)%couleur/pi+&
                                BRDF(wi,rayon%direction,N,scenes%spheres(sphere_id)%phong)*scenes%spheres(sphere_id)%ks&
                                *scenes%spheres(sphere_id)%couleur)
        END IF

    END SUBROUTINE eclairage_direct_path

    SUBROUTINE eclairage_indirect_path(intensite_pixel,scenes,P,N,sphere_id,nb_rebond_max,rayon)

        IMPLICIT NONE
        TYPE(scene), INTENT(IN) :: scenes
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(millieu) :: environement
        INTEGER, INTENT(IN) :: nb_rebond_max
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3) :: N, P, R
        DOUBLE PRECISION, DIMENSION(3) :: direction_aleatoir, intensite_pixel_ajout
        TYPE(ray) :: rayon_aleatoire
        DOUBLE PRECISION :: p1, proba, proba_global, proba_phong
        INTEGER :: sphere_id
        LOGICAL :: diffuse

        CALL RANDOM_NUMBER (proba)
        R=reflect(N,rayon%direction)
        p1=1-scenes%spheres(sphere_id)%ks

        if(proba<p1)THEN
            diffuse=.TRUE.
            direction_aleatoir=vect_random(N)
        ELSE
            diffuse=.FALSE.
            direction_aleatoir=vect_phong_random(R,scenes%spheres(sphere_id)%phong)
            IF(DOT_PRODUCT(direction_aleatoir,N)<0 .OR. DOT_PRODUCT(direction_aleatoir,R)<0)THEN
                intensite_pixel=0
                RETURN 
            END IF
        END IF

        rayon_aleatoire%origine=P+0.01*N
        rayon_aleatoire%direction=direction_aleatoir
        CALL get_color_path(rayon_aleatoire, scenes, intensite_pixel_ajout, nb_rebond_max-1,environement,.FALSE.)
        proba_phong=(scenes%spheres(sphere_id)%phong+1)/(2.*pi)*DOT_PRODUCT(R,direction_aleatoir)**scenes%spheres(sphere_id)%phong
        proba_global=p1*DOT_PRODUCT(N,direction_aleatoir)/pi + (1-p1)*proba_phong

        IF(diffuse)THEN
            intensite_pixel=intensite_pixel+intensite_pixel_ajout*scenes%spheres(sphere_id)%couleur* &
                            & DOT_PRODUCT(N,direction_aleatoir)/pi/proba_global
        ELSE
            intensite_pixel=intensite_pixel+intensite_pixel_ajout* DOT_PRODUCT(N,direction_aleatoir)* &
                            & BRDF(direction_aleatoir,rayon%direction,N,scenes%spheres(sphere_id)%phong) &
                            & *scenes%spheres(sphere_id)%ks*scenes%spheres(sphere_id)%couleur/proba_global
        END IF

    END SUBROUTINE eclairage_indirect_path

    RECURSIVE SUBROUTINE get_color_path(rayon, scenes, intensite_pixel, nb_rebond_max, environement, show_light)

        IMPLICIT NONE
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(scene), INTENT(IN) :: scenes
        TYPE(millieu), INTENT(INOUT) :: environement
        INTEGER, INTENT(IN) :: nb_rebond_max
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        LOGICAL, INTENT(IN) :: show_light
        DOUBLE PRECISION, DIMENSION(3) :: N, P
        LOGICAL :: bool
        INTEGER :: sphere_id
        DOUBLE PRECISION :: t

        IF(nb_rebond_max==0)THEN
            intensite_pixel=0
            RETURN
        END IF

        intensite_pixel=0

        CALL intersection_scene_path(rayon,scenes,bool, N, P, sphere_id, t)
        IF (bool)THEN

            IF(sphere_id==1 .AND. show_light)THEN
                intensite_pixel=scenes%spheres(sphere_id)%couleur*scenes%lum%intensite_lumiere
                RETURN
            ELSE

                IF(scenes%spheres(sphere_id)%Miror)THEN
                    IF(nb_rebond_max/=0)THEN
                        CALL miroir_path(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)
                    END IF
                ELSE
                    IF(scenes%spheres(sphere_id)%transparent)THEN
                        IF(nb_rebond_max/=0)THEN
                            CALL transparent_path(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)
                        END IF
                    ELSE 
                        IF(nb_rebond_max/=0)THEN

                            CALL eclairage_direct_path(intensite_pixel,scenes,P,N, sphere_id,rayon)

                            CALL eclairage_indirect_path(intensite_pixel,scenes,P,N,sphere_id,nb_rebond_max,rayon)

                        END IF
                    END IF

                END IF

            END IF

        END IF

    END SUBROUTINE get_color_path

END MODULE construct_path