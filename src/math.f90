MODULE math

    IMPLICIT NONE

    !pi in double precision
    DOUBLE PRECISION, PARAMETER :: pi=ACOS(-1.d0)

    CONTAINS

    !function that calculates the dot product between a and b
    FUNCTION dot(a,b)

        IMPLICIT NONE 

        DOUBLE PRECISION, DIMENSION(3) :: a, b
        DOUBLE PRECISION :: dot

        dot = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)

    END FUNCTION dot

    !function that calculates the cross product between a and b
    FUNCTION cross(a,b)

        IMPLICIT NONE 

        DOUBLE PRECISION, DIMENSION(3) :: a, b
        DOUBLE PRECISION, DIMENSION(3) :: cross

        cross(1) = a(2)*b(3) - b(2)*a(3)
        cross(2) = -(a(1)*b(3) - b(1)*a(3))
        cross(3) = a(1)*b(2) - b(1)*a(2)

    END FUNCTION cross

    !function that calculates the squared norm of a
    FUNCTION norm(a)

        IMPLICIT NONE 

        DOUBLE PRECISION, DIMENSION(3) :: a
        DOUBLE PRECISION :: norm

        norm = a(1)**2 + a(2)**2 + a(3)**2
    
    END FUNCTION norm

    !function that transforms a into a unit vector
    FUNCTION normalise(a)

        IMPLICIT NONE 

        DOUBLE PRECISION, DIMENSION(3) :: a
        DOUBLE PRECISION, DIMENSION(3) :: normalise

            normalise = a / SQRT(norm(a))
    
    END FUNCTION normalise

    FUNCTION vect_random(N)
        
        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3) :: vect_random
        DOUBLE PRECISION, DIMENSION(3) :: N
        DOUBLE PRECISION :: r1, r2
        DOUBLE PRECISION, DIMENSION(3) :: direction_aleatoir_local, vect_aleatoir, tangent1, tangent2

        CALL RANDOM_NUMBER(r1)
        CALL RANDOM_NUMBER(r2)

        direction_aleatoir_local=(/COS(2*pi*r1)*SQRT(1-r2),SIN(2*pi*r1)*SQRT(1-r2),SQRT(r2)/)
        CALL RANDOM_NUMBER(vect_aleatoir) ; vect_aleatoir=vect_aleatoir-0.5
        tangent1=cross(N,vect_aleatoir) ; tangent1=normalise(tangent1)
        tangent2=cross(tangent1,N)
        vect_random=direction_aleatoir_local(3)*N + &
                           & direction_aleatoir_local(1)*tangent1 + &
                           & direction_aleatoir_local(2)*tangent2

    END FUNCTION vect_random

    FUNCTION vect_phong_random(R,phong)
        
        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3) :: R,vect_phong_random
        DOUBLE PRECISION :: phong, facteur, r1, r2
        DOUBLE PRECISION, DIMENSION(3) :: direction_aleatoir_local, vect_aleatoir, tangent1, tangent2

        CALL RANDOM_NUMBER(r1)
        CALL RANDOM_NUMBER(r2)

        facteur=SQRT(1-r2**(2./(phong+1)))
        direction_aleatoir_local=(/COS(2*pi*r1)*facteur,SIN(2*pi*r1)*facteur,r2**(1./(phong+1))/)
        CALL RANDOM_NUMBER(vect_aleatoir) ; vect_aleatoir=vect_aleatoir-0.5
        tangent1=cross(R,vect_aleatoir) ; tangent1=normalise(tangent1)
        tangent2=cross(tangent1,R)
        vect_phong_random=direction_aleatoir_local(3)*R + &
                           & direction_aleatoir_local(1)*tangent1 + &
                           & direction_aleatoir_local(2)*tangent2

    END FUNCTION vect_phong_random

    FUNCTION reflect(N,wo)

        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3) :: N, Wo, reflect

        reflect=wo-2*DOT_PRODUCT(wo,N)*N
        
    END FUNCTION

    FUNCTION BRDF(wi,wo,N,phong)

        IMPLICIT NONE
        DOUBLE PRECISION, DIMENSION(3) :: N, Wo, wi, reflechie
        DOUBLE PRECISION :: phong, BRDF

        reflechie=reflect(N,wo)
        BRDF=(DOT_PRODUCT(reflechie,wi)**phong)*(phong+2)/(2*pi)

    END FUNCTION BRDF


END MODULE math