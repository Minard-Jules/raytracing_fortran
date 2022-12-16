MODULE type


    TYPE ray
        DOUBLE PRECISION, DIMENSION(3) :: origine, direction
    END TYPE ray

    TYPE millieu
        DOUBLE PRECISION :: n1, n2
    END TYPE millieu


    TYPE sphere 
        DOUBLE PRECISION, DIMENSION(3) :: origine
        DOUBLE PRECISION :: rayon
        DOUBLE PRECISION, DIMENSION(3) :: couleur
        LOGICAL :: Miror=.FALSE.
        LOGICAL :: transparent=.FALSE.
        DOUBLE PRECISION :: phong=1000.d0
        DOUBLE PRECISION :: ks=0.d0
    END TYPE sphere


    TYPE lumiere
        DOUBLE PRECISION, DIMENSION(3) :: position_lumiere
        DOUBLE PRECISION :: intensite_lumiere
    END TYPE lumiere

    TYPE scene
        TYPE(sphere), DIMENSION(:), ALLOCATABLE :: spheres
        TYPE(lumiere) :: lum
        INTEGER :: nb_scene
    END TYPE scene


    TYPE DIMENSION
       INTEGER :: width, height
    END TYPE DIMENSION

END MODULE type