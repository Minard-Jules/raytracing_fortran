MODULE construct_ray

    USE math
    USE type

    CONTAINS


    SUBROUTINE intersection_ray(rayon, sphere1, bool, N, P, t)

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

    END SUBROUTINE intersection_ray

    SUBROUTINE intersection_scene_ray(rayon, scenes, inter_global, global_N, global_P, sphere_id, t_min)

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

            CALL intersection_ray(rayon, scenes%spheres(i), bool, local_N, local_P, t)
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

    END SUBROUTINE intersection_scene_ray

    SUBROUTINE miroir_ray(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)

        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT):: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3), INTENT(IN) :: N, P
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(millieu), INTENT(INOUT) :: environement
        TYPE(scene), INTENT(IN) :: scenes
        INTEGER, INTENT(IN) :: nb_rebond_max
        TYPE(ray) :: rayon_miroir

        rayon_miroir%origine=P+0.01*N
        rayon_miroir%direction=rayon%direction-2*DOT_PRODUCT(rayon%direction,N)*N
        CALL get_color_ray(rayon_miroir, scenes, intensite_pixel, nb_rebond_max-1,environement)

    END SUBROUTINE miroir_ray

    SUBROUTINE transparent_ray(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)

        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3), INTENT(INOUT) :: N, P
        DOUBLE PRECISION, DIMENSION(3) :: N_transparent, tmp
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(millieu), INTENT(INOUT) :: environement
        TYPE(scene), INTENT(IN) :: scenes
        INTEGER, INTENT(IN) :: nb_rebond_max
        TYPE(ray) :: rayon_refracte
        DOUBLE PRECISION :: n1, n2, radical

        n1=environement%n1
        n2=environement%n2
        N_transparent=N

        IF(DOT_PRODUCT(rayon%direction,N)>0)THEN
            n1=environement%n2
            n2=environement%n1
            N_transparent=-N
        END IF
        radical=1-(n1/n2)**2*(1-DOT_PRODUCT(N_transparent,rayon%direction)**2)
        IF(radical>0)THEN
            tmp=DOT_PRODUCT(rayon%direction,N_transparent)*N_transparent
            rayon_refracte%origine=P-0.01*N_transparent
            rayon_refracte%direction=(n1/n2)*(rayon%direction-tmp) - N_transparent*SQRT(radical)
            CALL get_color_ray(rayon_refracte, scenes, intensite_pixel, nb_rebond_max-1,environement)
        END IF

    END SUBROUTINE transparent_ray

    SUBROUTINE eclairage_direct_ray(intensite_pixel,scenes,P,N,sphere_id)

        IMPLICIT NONE
        TYPE(scene), INTENT(IN) :: scenes
        INTEGER, INTENT(IN) :: sphere_id
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3), INTENT(IN) :: N, P
        DOUBLE PRECISION, DIMENSION(3) :: tmp1, tmp2, N_light, P_light
        LOGICAL :: inter_light
        TYPE(ray) :: rayon_light
        INTEGER :: sphere_id_light
        DOUBLE PRECISION :: d_light2, t_light


        tmp1=scenes%lum%position_lumiere-P
        rayon_light%origine=P+0.01*N
        rayon_light%direction=normalise((tmp1))

        CALL intersection_scene_ray(rayon_light,scenes,inter_light, N_light, P_light, sphere_id_light, t_light)
        d_light2=norm(tmp1)
        IF(inter_light .AND. t_light**2<d_light2)THEN
            intensite_pixel=(/0,0,0/)
        ELSE
            tmp2=scenes%spheres(sphere_id)%couleur*scenes%lum%intensite_lumiere
            intensite_pixel=tmp2*MAX(0.,DOT_PRODUCT(normalise(tmp1),N))/d_light2
        END IF

    END SUBROUTINE eclairage_direct_ray

    RECURSIVE SUBROUTINE get_color_ray(rayon, scenes, intensite_pixel, nb_rebond_max, environement)

        IMPLICIT NONE
        TYPE(ray), INTENT(IN) :: rayon
        TYPE(scene), INTENT(IN) :: scenes
        TYPE(millieu), INTENT(INOUT) :: environement
        INTEGER, INTENT(IN) :: nb_rebond_max
        DOUBLE PRECISION, DIMENSION(3), INTENT(OUT) :: intensite_pixel
        DOUBLE PRECISION, DIMENSION(3) :: N, P
        LOGICAL :: bool
        INTEGER :: sphere_id
        DOUBLE PRECISION :: t

        IF(nb_rebond_max==0)THEN
            intensite_pixel=0
            RETURN
        END IF

        intensite_pixel=0

        CALL intersection_scene_ray(rayon,scenes,bool, N, P, sphere_id, t)

        IF (bool)THEN
            IF(scenes%spheres(sphere_id)%Miror)THEN
                
                CALL miroir_ray(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)

            ELSE
                IF(scenes%spheres(sphere_id)%transparent)THEN

                    CALL transparent_ray(intensite_pixel,rayon,scenes,nb_rebond_max,environement,P,N)

                ELSE 

                    CALL eclairage_direct_ray(intensite_pixel,scenes,P,N,sphere_id)

                END IF

            END IF


        END IF

    END SUBROUTINE get_color_ray

END MODULE construct_ray