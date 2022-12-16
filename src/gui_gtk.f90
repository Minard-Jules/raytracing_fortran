MODULE all_MODULE_gtk

    USE, INTRINSIC :: iso_c_BINDing
    USE gtk, ONLY : gtk_window_destroy, gtk_application_window_new, g_signal_connect, gtk_window_set_title, &
        & gtk_window_set_default_size, G_APPLICATION_FLAGS_NONE, gtk_application_new, gtk_widget_show, &
        & gtk_window_set_mnemonics_visible, gtk_window_set_child, gtk_box_new, gtk_scrolled_window_new, & 
        & gtk_scrolled_window_set_policy, GTK_POLICY_AUTOMATIC, GTK_ORIENTATION_VERTICAL, GTK_ORIENTATION_HORIZONTAL, &
        & gtk_scrolled_window_set_has_frame, GTK_CORNER_TOP_RIGHT, gtk_scrolled_window_set_placement, &
        & gtk_button_new_with_mnemonic, gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
        & gtk_drawing_area_set_draw_func, gtk_drawing_area_new, gtk_label_new_with_mnemonic, gtk_notebook_new, &
        & gtk_widget_set_vexpand, gtk_notebook_append_page, GDK_COLORSPACE_RGB, gtk_grid_set_column_homogeneous, &
        & gtk_text_view_new, gtk_text_view_get_buffer, GTK_POLICY_ALWAYS, TRUE, FALSE, gtk_grid_new, &
        & gtk_grid_set_row_homogeneous, gtk_expander_set_child, gtk_expander_set_expanded, gtk_box_append, &
        & gtk_expander_new_with_mnemonic, gtk_adjustment_new, gtk_expander_set_expanded, gtk_grid_attach, &
        & gtk_toggle_button_get_active
    USE g, ONLY : g_application_run, g_object_unref
    USE gdk_pixbuf, ONLY: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
    USE gtk_hl_container
    USE gtk_hl_button
    USE gtk_hl_spin_slider
    USE gtk_hl_entry
    USE gtk_draw_hl
    USE OMP_LIB

END MODULE all_MODULE_gtk

MODULE global_widgets
    
    USE, INTRINSIC :: iso_c_BINDing

    IMPLICIT NONE

    !light
    TYPE(c_ptr) :: spin_x, spin_y, spin_z, spin_light_intensity, spin_light_rayon, red_spin_light, &
    & blue_spin_light, green_spin_light, spin_nb_rayon, spin_thread
    !left wall
    TYPE(c_ptr) :: spin_left_wall, toggle_miror_left_wall, toggle_transparent_left_wall, red_spin_left_wall, &
    & green_spin_left_wall, blue_spin_left_wall, refractive_index_spin_left_wall
    !right wall
    TYPE(c_ptr) :: spin_right_wall, toggle_miror_right_wall, toggle_transparent_right_wall, red_spin_right_wall, &
    & green_spin_right_wall, blue_spin_right_wall, refractive_index_spin_right_wall
    !top wall
    TYPE(c_ptr) :: spin_top_wall, toggle_miror_top_wall, toggle_transparent_top_wall, red_spin_top_wall, &
    & green_spin_top_wall, blue_spin_top_wall, refractive_index_spin_top_wall
    !below wall
    TYPE(c_ptr) :: spin_below_wall, toggle_miror_below_wall, toggle_transparent_below_wall, red_spin_below_wall, &
    & green_spin_below_wall, blue_spin_below_wall, refractive_index_spin_below_wall
    !bottom wall
    TYPE(c_ptr) :: spin_bottom_wall, toggle_miror_bottom_wall, toggle_transparent_bottom_wall, red_spin_bottom_wall, &
        & green_spin_bottom_wall, blue_spin_bottom_wall, refractive_index_spin_bottom_wall
    !sphere
    TYPE(c_ptr) :: spin_sphere_x, spin_sphere_y, spin_sphere_z, spin_rayon_sphere, &
        & toggle_miror_sphere, toggle_transparent_sphere, red_spin_sphere, green_spin_sphere, blue_spin_sphere, &
        & refractive_index_spin_sphere, phong_spin_sphere, ks_spin_sphere
    INTEGER, PARAMETER :: pixwidth  = 1024, pixheight = 1024
    TYPE(c_ptr) :: buffer, my_drawing_area, my_pixbuf
    CHARACTER(len=80) :: string
    CHARACTER(kind=c_char), DIMENSION(:), POINTER :: pixel
    INTEGER(c_int) :: nch, rowstride
    LOGICAL :: computing = .false.

END MODULE global_widgets


MODULE FUNCTION_gtk

    USE all_MODULE_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: slide_x, slide_y, slide_z, slide_left_wall, slide_right_wall, slide_top_wall, &
        & slide_below_wall, slide_bottom_wall, bound_upper, bound_below_wall, bound_bottom_wall, bound_left_wall, &
        & bound_right_wall, bound_top_wall, bound_lower, slide_sphere_x, slide_sphere_y, slide_sphere_z, &
        & bound_upper_sphere, bound_lower_sphere

    CONTAINS

    !spinner
    SUBROUTINE spinner(widget, gdata) BIND(c)

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(c_double) :: val
        REAL,DIMENSION(:),POINTER :: widgetf, spinfx, spinfy, spinfz, spinf_left_wall, spinf_right_wall, &
            & spinf_top_wall, spinf_below_wall, spinf_bottom_wall, spin_spherefx, spin_spherefy, spin_spherefz

        val = hl_gtk_spin_button_get_value(widget)

        CALL c_f_pointer(widget, widgetf,[12])

        CALL c_f_pointer(spin_x, spinfx,[12])
        CALL c_f_pointer(spin_y, spinfy,[12])
        CALL c_f_pointer(spin_z, spinfz,[12])
        CALL c_f_pointer(spin_left_wall, spinf_left_wall,[12])
        CALL c_f_pointer(spin_right_wall, spinf_right_wall,[12])
        CALL c_f_pointer(spin_top_wall, spinf_top_wall,[12])
        CALL c_f_pointer(spin_below_wall, spinf_below_wall,[12])
        CALL c_f_pointer(spin_bottom_wall, spinf_bottom_wall,[12])
        CALL c_f_pointer(spin_sphere_x, spin_spherefx,[12])
        CALL c_f_pointer(spin_sphere_y, spin_spherefy,[12])
        CALL c_f_pointer(spin_sphere_z, spin_spherefz,[12])



        IF (ASSOCIATED(widgetf, spinfx))THEN
            CALL hl_gtk_slider_set_VALUE(slide_x, val)
        ELSE IF (ASSOCIATED(widgetf, spinfy))THEN
            CALL hl_gtk_slider_set_VALUE(slide_y, val)
        ELSE IF (ASSOCIATED(widgetf, spinfz))THEN
            CALL hl_gtk_slider_set_VALUE(slide_z, val)   
        ELSE IF (ASSOCIATED(widgetf, spinf_left_wall))THEN
            CALL hl_gtk_slider_set_VALUE(slide_left_wall, val)               
        ELSE IF (ASSOCIATED(widgetf, spinf_right_wall))THEN
            CALL hl_gtk_slider_set_VALUE(slide_right_wall, val)            
        ELSE IF (ASSOCIATED(widgetf, spinf_top_wall))THEN
            CALL hl_gtk_slider_set_VALUE(slide_top_wall, val)           
        ELSE IF (ASSOCIATED(widgetf, spinf_below_wall))THEN
            CALL hl_gtk_slider_set_VALUE(slide_below_wall, val)           
        ELSE IF (ASSOCIATED(widgetf, spinf_bottom_wall))THEN
            CALL hl_gtk_slider_set_VALUE(slide_bottom_wall, val)         
        ELSE IF (ASSOCIATED(widgetf, spin_spherefx))THEN
            CALL hl_gtk_slider_set_VALUE(slide_sphere_x, val)        
        ELSE IF (ASSOCIATED(widgetf, spin_spherefy))THEN
            CALL hl_gtk_slider_set_VALUE(slide_sphere_y, val)          
        ELSE IF (ASSOCIATED(widgetf, spin_spherefz))THEN
            CALL hl_gtk_slider_set_VALUE(slide_sphere_z, val)          
        END IF
        
    END SUBROUTINE spinner

    !slider
    SUBROUTINE slider(widget, gdata) BIND(c)

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(c_double) :: val
        REAL,DIMENSION(:),POINTER :: widgetf, slidefx, slidefy, slidefz, slidef_left_wall, slidef_right_wall, &
            & slidef_top_wall, slidef_below_wall, slidef_bottom_wall, slide_spherefx, slide_spherefy, slide_spherefz

        val = hl_gtk_slider_get_VALUE(widget)

        CALL c_f_pointer(widget, widgetf,[12])

        CALL c_f_pointer(slide_x, slidefx,[12])
        CALL c_f_pointer(slide_y, slidefy,[12])
        CALL c_f_pointer(slide_z, slidefz,[12])
        CALL c_f_pointer(slide_left_wall, slidef_left_wall,[12])
        CALL c_f_pointer(slide_right_wall, slidef_right_wall,[12])
        CALL c_f_pointer(slide_top_wall, slidef_top_wall,[12])
        CALL c_f_pointer(slide_below_wall, slidef_below_wall,[12])
        CALL c_f_pointer(slide_bottom_wall, slidef_bottom_wall,[12])
        CALL c_f_pointer(slide_sphere_x, slide_spherefx,[12])
        CALL c_f_pointer(slide_sphere_y, slide_spherefy,[12])
        CALL c_f_pointer(slide_sphere_z, slide_spherefz,[12])

        IF (ASSOCIATED(widgetf, slidefx))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_x, val)
        ELSE IF (ASSOCIATED(widgetf, slidefy))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_y, val)
        ELSE IF (ASSOCIATED(widgetf, slidefz))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_z, val)           
        ELSE IF (ASSOCIATED(widgetf, slidef_left_wall))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_left_wall, val)                       
        ELSE IF (ASSOCIATED(widgetf, slidef_right_wall))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_right_wall, val)           
        ELSE IF (ASSOCIATED(widgetf, slidef_top_wall))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_top_wall, val)           
        ELSE IF (ASSOCIATED(widgetf, slidef_below_wall))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_below_wall, val)           
        ELSE IF (ASSOCIATED(widgetf, slidef_bottom_wall))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_bottom_wall, val)
        ELSE IF (ASSOCIATED(widgetf, slide_spherefx))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_sphere_x, val)           
        ELSE IF (ASSOCIATED(widgetf, slide_spherefy))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_sphere_y, val)           
        ELSE IF (ASSOCIATED(widgetf, slide_spherefz))THEN
            CALL hl_gtk_spin_button_set_VALUE(spin_sphere_z, val)           
        END IF

    END SUBROUTINE slider

    !set upper
    SUBROUTINE set_upper(widget, gdata) BIND(c)

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(c_double) :: ulim
        CHARACTER(len=20)   :: slim
        INTEGER :: ios
        REAL,DIMENSION(:),POINTER :: bound_upperf, bound_left_wallf, bound_right_wallf, bound_top_wallf, bound_below_wallf, &
            & bound_bottom_wallf, widgetf, bound_upper_spheref


        CALL hl_gtk_entry_get_text(widget, slim)

        READ(slim, *, iostat=ios) ulim

        CALL c_f_pointer(widget, widgetf,[12])

        CALL c_f_pointer(bound_upper, bound_upperf,[12])
        CALL c_f_pointer(bound_left_wall, bound_left_wallf,[12])
        CALL c_f_pointer(bound_right_wall, bound_right_wallf,[12])
        CALL c_f_pointer(bound_top_wall, bound_top_wallf,[12])
        CALL c_f_pointer(bound_below_wall, bound_below_wallf,[12])
        CALL c_f_pointer(bound_bottom_wall, bound_bottom_wallf,[12])
        CALL c_f_pointer(bound_upper_sphere, bound_upper_spheref,[12])

        IF (ASSOCIATED(widgetf, bound_upperf))THEN
            CALL hl_gtk_slider_set_range(slide_x, upper=ulim)
            CALL hl_gtk_slider_set_range(slide_y, upper=ulim)
            CALL hl_gtk_slider_set_range(slide_z, upper=ulim)

            CALL hl_gtk_spin_button_set_range(spin_x, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_y, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_z, upper=ulim)
        ELSE IF (ASSOCIATED(widgetf, bound_left_wallf))THEN
            CALL hl_gtk_slider_set_range(slide_left_wall, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_left_wall, upper=ulim)
        ELSE IF (ASSOCIATED(widgetf, bound_right_wallf))THEN
            CALL hl_gtk_slider_set_range(slide_right_wall, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_right_wall, upper=ulim)
        ELSE IF (ASSOCIATED(widgetf, bound_top_wallf))THEN
            CALL hl_gtk_slider_set_range(slide_top_wall, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_top_wall, upper=ulim)
        ELSE IF (ASSOCIATED(widgetf, bound_below_wallf))THEN
            CALL hl_gtk_slider_set_range(slide_below_wall, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_below_wall, upper=ulim)
        ELSE IF (ASSOCIATED(widgetf, bound_bottom_wallf))THEN
            CALL hl_gtk_slider_set_range(slide_bottom_wall, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_bottom_wall, upper=ulim)
        ELSE IF (ASSOCIATED(widgetf, bound_upper_spheref))THEN
            CALL hl_gtk_slider_set_range(slide_sphere_x, upper=ulim)
            CALL hl_gtk_slider_set_range(slide_sphere_y, upper=ulim)
            CALL hl_gtk_slider_set_range(slide_sphere_z, upper=ulim)

            CALL hl_gtk_spin_button_set_range(spin_sphere_x, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_sphere_y, upper=ulim)
            CALL hl_gtk_spin_button_set_range(spin_sphere_z, upper=ulim)
        END IF

    END SUBROUTINE set_upper

    !set lower
    SUBROUTINE set_lower(widget, gdata) BIND(c)

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        REAL(c_double) :: llim
        CHARACTER(len=20)   :: slim
        INTEGER :: ios
        REAL,DIMENSION(:),POINTER :: bound_lowerf, widgetf, bound_lower_spheref

        CALL hl_gtk_entry_get_text(widget, slim)

        READ(slim, *, iostat=ios) llim

        CALL c_f_pointer(widget, widgetf,[12])

        CALL c_f_pointer(bound_lower, bound_lowerf,[12])
        CALL c_f_pointer(bound_lower_sphere, bound_lower_spheref,[12])

        IF (ASSOCIATED(widgetf, bound_lowerf))THEN
            CALL hl_gtk_slider_set_range(slide_x, lower=llim)
            CALL hl_gtk_slider_set_range(slide_y, lower=llim)
            CALL hl_gtk_slider_set_range(slide_z, lower=llim)

            CALL hl_gtk_spin_button_set_range(spin_x, lower=llim)
            CALL hl_gtk_spin_button_set_range(spin_y, lower=llim)
            CALL hl_gtk_spin_button_set_range(spin_z, lower=llim)
        ELSE IF (ASSOCIATED(widgetf, bound_lower_spheref))THEN
            CALL hl_gtk_slider_set_range(slide_sphere_x, lower=llim)
            CALL hl_gtk_slider_set_range(slide_sphere_y, lower=llim)
            CALL hl_gtk_slider_set_range(slide_sphere_z, lower=llim)

            CALL hl_gtk_spin_button_set_range(spin_sphere_x, lower=llim)
            CALL hl_gtk_spin_button_set_range(spin_sphere_y, lower=llim)
            CALL hl_gtk_spin_button_set_range(spin_sphere_z, lower=llim)
        END IF

    END SUBROUTINE set_lower

    LOGICAL FUNCTION boolresult(val) 

        IMPLICIT NONE

        INTEGER :: val

        IF(val==1)THEN
            boolresult=.TRUE.
        ELSE IF(val==0)THEN
            boolresult=.FALSE.
        END IF

    END FUNCTION boolresult

    RECURSIVE SUBROUTINE firstbutton (widget, gdata ) BIND(c)

        USE global_widgets
        USE construct_ray

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        TYPE(scene) :: scenes
        TYPE(millieu):: environement
        TYPE(DIMENSION):: dim
        TYPE(sphere):: sphere1, sphere2, sphere3, sphere4, sphere5, sphere6
        INTEGER :: ok
        DOUBLE PRECISION :: tmp

        IF(.NOT. computing)THEN

            scenes%nb_scene=6
            ALLOCATE(scenes%spheres(scenes%nb_scene), stat=ok)
            IF (ok /= 0) STOP "Pblm allocation scenes%spheres/main_program"

            !initialiseation de la taille de l'image width*height
            dim%width=pixwidth
            dim%height=pixheight
            

            !initialisation de la lumiére : position, intensité
            scenes%lum%position_lumiere=(/hl_gtk_spin_button_get_VALUE(spin_x), &
                                        & hl_gtk_spin_button_get_VALUE(spin_y), &
                                        & hl_gtk_spin_button_get_VALUE(spin_z)/) 
            scenes%lum%intensite_lumiere=hl_gtk_spin_button_get_VALUE(spin_light_intensity)

            !initialisation de la sphére : origine, rayon, couleur
            sphere1%origine=(/hl_gtk_spin_button_get_VALUE(spin_sphere_x), &
                            & hl_gtk_spin_button_get_VALUE(spin_sphere_y), &
                            & hl_gtk_spin_button_get_VALUE(spin_sphere_z)/)  
            sphere1%rayon=hl_gtk_spin_button_get_VALUE(spin_rayon_sphere)
            sphere1%couleur=(/hl_gtk_spin_button_get_VALUE(red_spin_sphere)/255, &
                            & hl_gtk_spin_button_get_VALUE(green_spin_sphere)/255, &
                            & hl_gtk_spin_button_get_VALUE(blue_spin_sphere)/255/)  
            sphere1%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_sphere))
            sphere1%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_sphere))
            environement%n2=gtk_spin_button_get_VALUE(refractive_index_spin_sphere)

            !initialisation du sol : origine, rayon, couleur
            sphere2%origine=(/INT(2000+hl_gtk_spin_button_get_VALUE(spin_below_wall)), &
                            & 0, &
                            & 0/) 
            sphere2%rayon=2000 
            sphere2%couleur=(/hl_gtk_spin_button_get_VALUE(red_spin_below_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(green_spin_below_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(blue_spin_below_wall)/255/) 
            sphere2%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_below_wall))
            sphere2%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_below_wall))

            !initialisation du plafon : origine, rayon, couleur
            sphere3%origine=(/INT(-2000-hl_gtk_spin_button_get_VALUE(spin_top_wall)), &
                            & 0, &
                            & 0/)
            sphere3%rayon=2000 
            sphere3%couleur=(/hl_gtk_spin_button_get_VALUE(red_spin_top_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(green_spin_top_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(blue_spin_top_wall)/255/) 
            sphere3%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_top_wall))
            sphere3%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_top_wall))

            !initialisation du mur droit : origine, rayon, couleur
            sphere4%origine=(/0, &
                            & INT(2000+hl_gtk_spin_button_get_VALUE(spin_right_wall)), &
                            & 0/) 
            sphere4%rayon=2000 
            sphere4%couleur=(/hl_gtk_spin_button_get_VALUE(red_spin_right_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(green_spin_right_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(blue_spin_right_wall)/255/)
            sphere4%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_right_wall))
            sphere4%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_right_wall))

            !initialisation du mur gauche : origine, rayon, couleur
            sphere5%origine=(/0, &
                            & INT(-2000-hl_gtk_spin_button_get_VALUE(spin_left_wall)), &
                            & 0/) 
            sphere5%rayon=2000 
            sphere5%couleur=(/hl_gtk_spin_button_get_VALUE(red_spin_left_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(green_spin_left_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(blue_spin_left_wall)/255/)
            sphere5%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_left_wall))
            sphere5%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_left_wall))

            !initialisation du mur fond : origine, rayon, couleur
            sphere6%origine=(/0, &
                            & 0, &
                            & INT(-2000-hl_gtk_spin_button_get_VALUE(spin_bottom_wall))/)
            sphere6%rayon=2000
            sphere6%couleur=(/hl_gtk_spin_button_get_VALUE(red_spin_bottom_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(green_spin_bottom_wall)/255, &
                            & hl_gtk_spin_button_get_VALUE(blue_spin_bottom_wall)/255/)
            sphere6%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_bottom_wall))
            sphere6%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_bottom_wall))

            scenes%spheres=(/sphere1,sphere2,sphere3,sphere4,sphere5,sphere6/)

            environement%n1=1

            CALL raytracing_set_ray( scenes, dim, environement )
        END IF

    END SUBROUTINE firstbutton

    RECURSIVE SUBROUTINE secondbutton (widget, gdata ) BIND(c)

        USE global_widgets
        USE construct_path

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        TYPE(scene) :: scenes
        TYPE(millieu):: environement
        TYPE(DIMENSION):: dim
        TYPE(sphere):: sphere1, sphere2, sphere3, sphere4, sphere5, sphere6, slum
        INTEGER :: ok
        DOUBLE PRECISION :: tmp

        IF(.NOT. computing)THEN

            scenes%nb_scene=7
            ALLOCATE(scenes%spheres(scenes%nb_scene), stat=ok)
            IF (ok /= 0) STOP "Pblm allocation scenes%spheres/main_program"

            !initialiseation de la taille de l'image width*height
            dim%width=pixwidth
            dim%height=pixheight

            slum%origine=(/hl_gtk_spin_button_get_value(spin_x), &
                        & hl_gtk_spin_button_get_value(spin_y), &
                        & hl_gtk_spin_button_get_value(spin_z)/)
            slum%rayon=hl_gtk_spin_button_get_value(spin_light_rayon)
            slum%couleur=(/hl_gtk_spin_button_get_value(red_spin_light), &
                        & hl_gtk_spin_button_get_value(green_spin_light), &
                        & hl_gtk_spin_button_get_value(blue_spin_light)/)   
            scenes%lum%intensite_lumiere=hl_gtk_spin_button_get_value(spin_light_intensity)

            !initialisation de la sphére : origine, rayon, couleur
            sphere1%origine=(/hl_gtk_spin_button_get_value(spin_sphere_x), &
                            & hl_gtk_spin_button_get_value(spin_sphere_y), &
                            & hl_gtk_spin_button_get_value(spin_sphere_z)/)  
            sphere1%rayon=hl_gtk_spin_button_get_value(spin_rayon_sphere)
            sphere1%couleur=(/hl_gtk_spin_button_get_value(red_spin_sphere), &
                            & hl_gtk_spin_button_get_value(green_spin_sphere), &
                            & hl_gtk_spin_button_get_value(blue_spin_sphere)/)  
            sphere1%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_sphere))
            sphere1%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_sphere))
            sphere1%ks=hl_gtk_spin_button_get_value(ks_spin_sphere)
            sphere1%phong=hl_gtk_spin_button_get_value(phong_spin_sphere)
            environement%n2=gtk_spin_button_get_VALUE(refractive_index_spin_sphere)

            !initialisation du sol : origine, rayon, couleur
            sphere2%origine=(/INT(2000+hl_gtk_spin_button_get_value(spin_below_wall)), &
                            & 0, &
                            & 0/) 
            sphere2%rayon=2000 
            sphere2%couleur=(/hl_gtk_spin_button_get_value(red_spin_below_wall), &
                            & hl_gtk_spin_button_get_value(green_spin_below_wall), &
                            & hl_gtk_spin_button_get_value(blue_spin_below_wall)/) 
            sphere2%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_below_wall))
            sphere2%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_below_wall))

            !initialisation du plafon : origine, rayon, couleur
            sphere3%origine=(/INT(-2000-hl_gtk_spin_button_get_value(spin_top_wall)), &
                            & 0, &
                            & 0/)
            sphere3%rayon=2000 
            sphere3%couleur=(/hl_gtk_spin_button_get_value(red_spin_top_wall), &
                            & hl_gtk_spin_button_get_value(green_spin_top_wall), &
                            & hl_gtk_spin_button_get_value(blue_spin_top_wall)/) 
            sphere3%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_top_wall))
            sphere3%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_top_wall))

            !initialisation du mur droit : origine, rayon, couleur
            sphere4%origine=(/0, &
                            & INT(2000+hl_gtk_spin_button_get_value(spin_right_wall)), &
                            & 0/) 
            sphere4%rayon=2000 
            sphere4%couleur=(/hl_gtk_spin_button_get_value(red_spin_right_wall), &
                            & hl_gtk_spin_button_get_value(green_spin_right_wall), &
                            & hl_gtk_spin_button_get_value(blue_spin_right_wall)/)
            sphere4%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_right_wall))
            sphere4%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_right_wall))

            !initialisation du mur gauche : origine, rayon, couleur
            sphere5%origine=(/0, &
                            & INT(-2000-hl_gtk_spin_button_get_value(spin_left_wall)), &
                            & 0/) 
            sphere5%rayon=2000 
            sphere5%couleur=(/hl_gtk_spin_button_get_value(red_spin_left_wall), &
                            & hl_gtk_spin_button_get_value(green_spin_left_wall), &
                            & hl_gtk_spin_button_get_value(blue_spin_left_wall)/)
            sphere5%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_left_wall))
            sphere5%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_left_wall))

            !initialisation du mur fond : origine, rayon, couleur
            sphere6%origine=(/0, &
                            & 0, &
                            & INT(-2000-hl_gtk_spin_button_get_value(spin_bottom_wall))/)
            sphere6%rayon=2000
            sphere6%couleur=(/hl_gtk_spin_button_get_value(red_spin_bottom_wall), &
                            & hl_gtk_spin_button_get_value(green_spin_bottom_wall), &
                            & hl_gtk_spin_button_get_value(blue_spin_bottom_wall)/)
            sphere6%transparent=boolresult(gtk_toggle_button_get_active(toggle_transparent_bottom_wall))
            sphere6%miror=boolresult(gtk_toggle_button_get_active(toggle_miror_bottom_wall))

            scenes%spheres=(/slum,sphere1,sphere2,sphere3,sphere4,sphere5,sphere6/)

            environement%n1=1

            CALL raytracing_set_path( scenes, dim, environement )
        END IF

    END SUBROUTINE secondbutton

    SUBROUTINE thirdbutton (widget, gdata) BIND(c)

        USE gdk_pixbuf, ONLY: gdk_pixbuf_savev
        USE global_widgets

        IMPLICIT NONE
    
        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, gdata
        INTEGER(c_int)                 :: cstatus
        LOGICAL :: dir_residu
        DOUBLE PRECISION :: numero


        CALL CPU_TIME(numero)

        !vérIFie si le dossier image_save existe et le créer sinon
        INQUIRE(file=trim('./image_save'), exist=dir_residu)
        IF(.NOT. dir_residu) THEN
            CALL execute_command_line("mkdir image_save")
        END IF
    

        IF(.NOT. computing)THEN
          cstatus = gdk_pixbuf_savev(my_pixbuf, "image_save/image_raytracing.png"//c_null_char, &
                    & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)
        END IF

    END SUBROUTINE thirdbutton

    SUBROUTINE raytracing_set_ray( scenes, dim, environement )

        USE construct_ray

        IMPLICIT NONE

        INTEGER :: i, j, ok
        TYPE(scene) :: scenes
        TYPE(millieu) :: environement
        TYPE(DIMENSION) :: dim
        TYPE(ray) :: rayon
        DOUBLE PRECISION, DIMENSION(3) :: intensite_pixel
        DOUBLE PRECISION :: fov=60*pi/180, seconds
        INTEGER :: red, green, blue, W, H, p

        computing = .TRUE.

        !commence le chronometre
        seconds = omp_get_wtime ()

        H=pixwidth
        W=pixwidth
            
        DO i=1, H-1 ; DO j=1, W-1
            
            !initial du rayon : origine, direction
            rayon%origine=(/0,0,0/)
            rayon%direction=normalise((/i-DBLE(W)/2, j-DBLE(H)/2,-DBLE(W)/(2*TAN(fov/2))/))
            CALL get_color_ray(rayon, scenes, intensite_pixel, 5, environement)
            intensite_pixel=intensite_pixel*50

            !enregistre l'intensité dans l'image
            red=INT(MIN(255.,MAX(0.,intensite_pixel(1)**(1./2.2))))
            green=INT(MIN(255.,MAX(0.,intensite_pixel(2)**(1./2.2))))
            blue=INT(MIN(255.,MAX(0.,intensite_pixel(3)**(1./2.2))))

            p = j * nch + i * rowstride + 1
            pixel(p)   = char(red)
            pixel(p+1) = char(green)
            pixel(p+2) = char(blue)

        END DO ; END DO

        CALL gtk_widget_queue_draw(my_drawing_area)

        computing = .false.

        !arrete le chronometre
        seconds = omp_get_wtime () - seconds
        WRITE(string, '("System time = ",F8.3, " s")') seconds
        CALL gtk_text_buffer_insert_at_cursor (buffer, string//C_NEW_LINE//c_null_char, -1_c_int)

    END SUBROUTINE raytracing_set_ray


    SUBROUTINE raytracing_set_path( scenes, dim, environement )

        USE construct_path

        IMPLICIT NONE

        INTEGER :: i, j, k, ok
        TYPE(scene) :: scenes
        TYPE(millieu) :: environement
        TYPE(DIMENSION) :: dim
        TYPE(ray) :: rayon
        DOUBLE PRECISION, DIMENSION(3) :: intensite_pixel, color
        DOUBLE PRECISION :: fov=60*pi/180, seconds, dx, dy, r1, r2, R
        INTEGER :: red, green, blue, W, H, p, nb_rayon=80, thread

        computing = .true.


        !commence le chronometre
        seconds = omp_get_wtime ()

        H=pixheight
        W=pixwidth
        nb_rayon=hl_gtk_spin_button_get_value(spin_nb_rayon)
        thread=hl_gtk_spin_button_get_value(spin_thread)

        !$OMP PARALLEL PRIVATE(rayon,color,intensite_pixel) NUM_THREADS(7)
        !$OMP DO
            
        DO i=1, H-1 ; DO j=1, W-1

            color=(/0,0,0/)

            DO k=0,nb_rayon

                !methode de box muller

                CALL RANDOM_NUMBER(r1) ; CALL RANDOM_NUMBER(r2)
                R=SQRT(-2*LOG(r1))
                dx=R*COS(2*pi*r2)
                dy=R*SIN(2*pi*r2)
                
                !initial du rayon : origine, direction
                rayon%origine=(/0,0,0/)
                rayon%direction=normalise((/i-DBLE(W)/2+0.5+dy, j-DBLE(H)/2+0.5+dx, -DBLE(W)/(2*TAN(fov/2))/))
                
                CALL get_color_path(rayon, scenes, intensite_pixel, 5, environement, .TRUE.)
                color=color+intensite_pixel/nb_rayon

            END DO

            !enregistre l'intensité dans l'image
            red=INT(MIN(255.,MAX(0.,color(1)**(1./2.2))))
            green=INT(MIN(255.,MAX(0.,color(2)**(1./2.2))))
            blue=INT(MIN(255.,MAX(0.,color(3)**(1./2.2))))

            p = j * nch + i * rowstride + 1
            pixel(p)   = char(red)
            pixel(p+1) = char(green)
            pixel(p+2) = char(blue)

        END DO ; END DO
        !$OMP END DO
        !$OMP END PARALLEL

        CALL gtk_widget_queue_draw(my_drawing_area)

        computing = .false.

        !arrete le chronometre
        seconds = omp_get_wtime () - seconds
        WRITE(string, '("System time = ",F8.3, " s")') seconds
        CALL gtk_text_buffer_insert_at_cursor (buffer, string//C_NEW_LINE//c_null_char, -1_c_int)

    END SUBROUTINE raytracing_set_path


END MODULE FUNCTION_gtk


MODULE mod_scene

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: table_scene, expander_scene
    TYPE(c_ptr) :: label_light, label_x, label_y, label_z, label_bound_upper, label_nb_sphere, &
        & spin_nb_sphere, label_bound_lower, label_light_intensity, label_nb_rayon, label_thread, &
        & label_light_couleur, label_blue_light, label_red_light, label_green_light, label_light_rayon

    CONTAINS


    SUBROUTINE activate_scene(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !scene
        label_light = gtk_label_new("light position"//c_null_char)
        !x
        label_x = gtk_label_new("x"//c_null_char)

        slide_x = hl_gtk_slider_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=-60.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_x = hl_gtk_spin_button_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=-60.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        !y
        label_y = gtk_label_new("y"//c_null_char)

        slide_y = hl_gtk_slider_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=-20.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_y = hl_gtk_spin_button_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=-20.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        !z
        label_z = gtk_label_new("z"//c_null_char)

        slide_z = hl_gtk_slider_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=0.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_z = hl_gtk_spin_button_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=0.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_upper = gtk_label_new("Upper bound :"//c_null_char)
        bound_upper = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_bound_lower = gtk_label_new("lower bound :"//c_null_char)
        bound_lower = hl_gtk_entry_new(VALUE="-100."//c_null_char, activate=c_funloc(set_lower))

        label_nb_sphere = gtk_label_new("number of sphere :"//c_null_char)
        spin_nb_sphere = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,5d0,1d0,0d0,0d0),1d0, 0_c_int)

        label_light_intensity = gtk_label_new("light intensitye :"//c_null_char)
        spin_light_intensity = gtk_spin_button_new(gtk_adjustment_new(5d9,0d0,1d10,1d0,0d0,0d0),1d0, 0_c_int)

        label_light_rayon = gtk_label_new("light ray :"//c_null_char)
        spin_light_rayon = gtk_spin_button_new(gtk_adjustment_new(5d0,0d0,5d8,1d0,0d0,0d0),1d0, 0_c_int)

        label_light_couleur = gtk_label_new("light color :"//c_null_char)
        label_red_light = gtk_label_new("Red"//c_null_char)
        label_green_light = gtk_label_new("Green"//c_null_char)
        label_blue_light = gtk_label_new("Blue"//c_null_char)

        red_spin_light = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_light = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_light = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_nb_rayon = gtk_label_new("number of ray :"//c_null_char)
        spin_nb_rayon = gtk_spin_button_new(gtk_adjustment_new(120d0,0d0,1000d0,1d0,0d0,0d0),1d0, 0_c_int)

        label_thread = gtk_label_new("number of thread use :"//c_null_char)
        spin_thread = gtk_spin_button_new(gtk_adjustment_new(7d0,0d0,12d0,1d0,0d0,0d0),1d0, 0_c_int)


        table_scene = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_scene, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_scene, FALSE)
        CALL gtk_grid_attach(table_scene, label_light, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_x, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_y, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_z, 2_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, slide_x, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, slide_y, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, slide_z, 2_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_x, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_y, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_z, 2_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_bound_upper, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, bound_upper, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_bound_lower, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, bound_lower, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_light_intensity, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_light_intensity, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_light_rayon, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_light_rayon, 1_c_int, 7_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_light_couleur, 1_c_int, 8_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_red_light, 0_c_int, 9_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_green_light, 1_c_int, 9_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_blue_light, 2_c_int, 9_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, red_spin_light, 0_c_int, 10_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, green_spin_light, 1_c_int, 10_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, blue_spin_light, 2_c_int, 10_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_nb_sphere, 0_c_int, 11_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_nb_sphere, 1_c_int, 11_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_nb_rayon, 0_c_int, 12_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_nb_rayon, 1_c_int, 12_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, label_thread, 0_c_int, 13_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_scene, spin_thread, 1_c_int, 13_c_int, 2_c_int, 1_c_int)

        expander_scene = gtk_expander_new_with_mnemonic ("scene:"//c_null_char)
        CALL gtk_expander_set_child(expander_scene, table_scene)
        CALL gtk_expander_set_expanded(expander_scene, FALSE)

        CALL gtk_box_append(box, expander_scene)

    END SUBROUTINE activate_scene

END MODULE mod_scene

MODULE mod_left_wall

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: label_left_wall, label_bound_left_wall, label_toggle_miror_left_wall, label_refractive_index_left_wall, &
        & label_toggle_transparent_left_wall, label_red_left_wall, label_green_left_wall, label_blue_left_wall
    TYPE(c_ptr) :: table_left_wall, expander_left_wall

    CONTAINS

    SUBROUTINE activate_left_wall(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !left wall
        slide_left_wall = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=40.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_left_wall = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=40.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_left_wall = gtk_label_new("Upper bound :"//c_null_char)
        bound_left_wall = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_toggle_miror_left_wall = gtk_label_new("left wall is a miror ?"//c_null_char)
        toggle_miror_left_wall = gtk_toggle_button_new ()

        label_toggle_transparent_left_wall = gtk_label_new("left wall is a transparent ?"//c_null_char)
        toggle_transparent_left_wall = gtk_toggle_button_new ()

        label_red_left_wall = gtk_label_new("Red"//c_null_char)
        label_green_left_wall = gtk_label_new("Green"//c_null_char)
        label_blue_left_wall = gtk_label_new("Blue"//c_null_char)

        red_spin_left_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_left_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_left_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_refractive_index_left_wall = gtk_label_new("refractive index"//c_null_char)
        refractive_index_spin_left_wall = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,3d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        table_left_wall = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_left_wall, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_left_wall, FALSE)
        CALL gtk_grid_attach(table_left_wall, slide_left_wall, 0_c_int, 0_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, spin_left_wall, 0_c_int, 1_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_bound_left_wall, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, bound_left_wall, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_toggle_miror_left_wall, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, toggle_miror_left_wall, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_toggle_transparent_left_wall, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, toggle_transparent_left_wall, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_red_left_wall, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_green_left_wall, 1_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_blue_left_wall, 2_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, red_spin_left_wall, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, green_spin_left_wall, 1_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, blue_spin_left_wall, 2_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, label_refractive_index_left_wall, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_left_wall, refractive_index_spin_left_wall, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

        expander_left_wall = gtk_expander_new_with_mnemonic ("left wall:"//c_null_char)
        CALL gtk_expander_set_child(expander_left_wall, table_left_wall)
        CALL gtk_expander_set_expanded(expander_left_wall, FALSE)

        CALL gtk_box_append(box, expander_left_wall)

    END SUBROUTINE activate_left_wall

END MODULE mod_left_wall

MODULE mod_right_wall

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: label_right_wall, label_bound_right_wall, label_toggle_miror_right_wall, label_refractive_index_right_wall, &
        & label_toggle_transparent_right_wall, label_red_right_wall, label_green_right_wall, label_blue_right_wall
    TYPE(c_ptr) :: table_right_wall, expander_right_wall

    CONTAINS

    SUBROUTINE activate_right_wall(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !left wall
        slide_right_wall = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=40.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_right_wall = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=40.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_right_wall = gtk_label_new("Upper bound :"//c_null_char)
        bound_right_wall = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_toggle_miror_right_wall = gtk_label_new("left wall is a miror ?"//c_null_char)
        toggle_miror_right_wall = gtk_toggle_button_new ()

        label_toggle_transparent_right_wall = gtk_label_new("left wall is a transparent ?"//c_null_char)
        toggle_transparent_right_wall = gtk_toggle_button_new ()

        label_red_right_wall = gtk_label_new("Red"//c_null_char)
        label_green_right_wall = gtk_label_new("Green"//c_null_char)
        label_blue_right_wall = gtk_label_new("Blue"//c_null_char)

        red_spin_right_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_right_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_right_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_refractive_index_right_wall = gtk_label_new("refractive index"//c_null_char)
        refractive_index_spin_right_wall = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,3d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        table_right_wall = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_right_wall, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_right_wall, FALSE)
        CALL gtk_grid_attach(table_right_wall, slide_right_wall, 0_c_int, 0_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, spin_right_wall, 0_c_int, 1_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_bound_right_wall, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, bound_right_wall, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_toggle_miror_right_wall, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, toggle_miror_right_wall, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_toggle_transparent_right_wall, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, toggle_transparent_right_wall, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_red_right_wall, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_green_right_wall, 1_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_blue_right_wall, 2_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, red_spin_right_wall, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, green_spin_right_wall, 1_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, blue_spin_right_wall, 2_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, label_refractive_index_right_wall, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_right_wall, refractive_index_spin_right_wall, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

        expander_right_wall = gtk_expander_new_with_mnemonic ("right wall:"//c_null_char)
        CALL gtk_expander_set_child(expander_right_wall, table_right_wall)
        CALL gtk_expander_set_expanded(expander_right_wall, FALSE)

        CALL gtk_box_append(box, expander_right_wall)

    END SUBROUTINE activate_right_wall

END MODULE mod_right_wall

MODULE mod_top_wall

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: label_top_wall, label_bound_top_wall, label_toggle_miror_top_wall, label_refractive_index_top_wall, &
        & label_toggle_transparent_top_wall, label_red_top_wall, label_green_top_wall, label_blue_top_wall
    TYPE(c_ptr) :: table_top_wall, expander_top_wall

    CONTAINS

    SUBROUTINE activate_top_wall(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !left wall
        slide_top_wall = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=100.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_top_wall = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=100.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_top_wall = gtk_label_new("Upper bound :"//c_null_char)
        bound_top_wall = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_toggle_miror_top_wall = gtk_label_new("left wall is a miror ?"//c_null_char)
        toggle_miror_top_wall = gtk_toggle_button_new ()

        label_toggle_transparent_top_wall = gtk_label_new("left wall is a transparent ?"//c_null_char)
        toggle_transparent_top_wall = gtk_toggle_button_new ()

        label_red_top_wall = gtk_label_new("Red"//c_null_char)
        label_green_top_wall = gtk_label_new("Green"//c_null_char)
        label_blue_top_wall = gtk_label_new("Blue"//c_null_char)

        red_spin_top_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_top_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_top_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_refractive_index_top_wall = gtk_label_new("refractive index"//c_null_char)
        refractive_index_spin_top_wall = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,3d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        table_top_wall = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_top_wall, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_top_wall, FALSE)
        CALL gtk_grid_attach(table_top_wall, slide_top_wall, 0_c_int, 0_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, spin_top_wall, 0_c_int, 1_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_bound_top_wall, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, bound_top_wall, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_toggle_miror_top_wall, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, toggle_miror_top_wall, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_toggle_transparent_top_wall, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, toggle_transparent_top_wall, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_red_top_wall, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_green_top_wall, 1_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_blue_top_wall, 2_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, red_spin_top_wall, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, green_spin_top_wall, 1_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, blue_spin_top_wall, 2_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, label_refractive_index_top_wall, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_top_wall, refractive_index_spin_top_wall, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

        expander_top_wall = gtk_expander_new_with_mnemonic ("top wall:"//c_null_char)
        CALL gtk_expander_set_child(expander_top_wall, table_top_wall)
        CALL gtk_expander_set_expanded(expander_top_wall, FALSE)

        CALL gtk_box_append(box, expander_top_wall)

    END SUBROUTINE activate_top_wall

END MODULE mod_top_wall

MODULE mod_below_wall

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: label_below_wall, label_bound_below_wall, label_toggle_miror_below_wall, label_refractive_index_below_wall, &
        & label_toggle_transparent_below_wall, label_red_below_wall, label_green_below_wall, label_blue_below_wall
    TYPE(c_ptr) :: table_below_wall, expander_below_wall

    CONTAINS

    SUBROUTINE activate_below_wall(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !left wall
        slide_below_wall = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=20.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_below_wall = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=20.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_below_wall = gtk_label_new("Upper bound :"//c_null_char)
        bound_below_wall = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_toggle_miror_below_wall = gtk_label_new("left wall is a miror ?"//c_null_char)
        toggle_miror_below_wall = gtk_toggle_button_new ()

        label_toggle_transparent_below_wall = gtk_label_new("left wall is a transparent ?"//c_null_char)
        toggle_transparent_below_wall = gtk_toggle_button_new ()

        label_red_below_wall = gtk_label_new("Red"//c_null_char)
        label_green_below_wall = gtk_label_new("Green"//c_null_char)
        label_blue_below_wall = gtk_label_new("Blue"//c_null_char)

        red_spin_below_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_below_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_below_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_refractive_index_below_wall = gtk_label_new("refractive index"//c_null_char)
        refractive_index_spin_below_wall = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,3d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        table_below_wall = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_below_wall, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_below_wall, FALSE)
        CALL gtk_grid_attach(table_below_wall, slide_below_wall, 0_c_int, 0_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, spin_below_wall, 0_c_int, 1_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_bound_below_wall, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, bound_below_wall, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_toggle_miror_below_wall, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, toggle_miror_below_wall, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_toggle_transparent_below_wall, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, toggle_transparent_below_wall, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_red_below_wall, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_green_below_wall, 1_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_blue_below_wall, 2_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, red_spin_below_wall, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, green_spin_below_wall, 1_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, blue_spin_below_wall, 2_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, label_refractive_index_below_wall, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_below_wall, refractive_index_spin_below_wall, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

        expander_below_wall = gtk_expander_new_with_mnemonic ("below wall:"//c_null_char)
        CALL gtk_expander_set_child(expander_below_wall, table_below_wall)
        CALL gtk_expander_set_expanded(expander_below_wall, FALSE)

        CALL gtk_box_append(box, expander_below_wall)

    END SUBROUTINE activate_below_wall

END MODULE mod_below_wall

MODULE mod_bottom_wall

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: label_bottom_wall, label_bound_bottom_wall, label_toggle_miror_bottom_wall, label_refractive_index_bottom_wall, &
        & label_toggle_transparent_bottom_wall, label_red_bottom_wall, label_green_bottom_wall, label_blue_bottom_wall
    TYPE(c_ptr) :: table_bottom_wall, expander_bottom_wall

    CONTAINS

    SUBROUTINE activate_bottom_wall(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !left wall
        slide_bottom_wall = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=100.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_bottom_wall = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=100.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_bottom_wall = gtk_label_new("Upper bound :"//c_null_char)
        bound_bottom_wall = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_toggle_miror_bottom_wall = gtk_label_new("left wall is a miror ?"//c_null_char)
        toggle_miror_bottom_wall = gtk_toggle_button_new ()

        label_toggle_transparent_bottom_wall = gtk_label_new("left wall is a transparent ?"//c_null_char)
        toggle_transparent_bottom_wall = gtk_toggle_button_new ()

        label_red_bottom_wall = gtk_label_new("Red"//c_null_char)
        label_green_bottom_wall = gtk_label_new("Green"//c_null_char)
        label_blue_bottom_wall = gtk_label_new("Blue"//c_null_char)

        red_spin_bottom_wall = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_bottom_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_bottom_wall = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_refractive_index_bottom_wall = gtk_label_new("refractive index"//c_null_char)
        refractive_index_spin_bottom_wall = gtk_spin_button_new(gtk_adjustment_new(1d0,0d0,3d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        table_bottom_wall = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_bottom_wall, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_bottom_wall, FALSE)
        CALL gtk_grid_attach(table_bottom_wall, slide_bottom_wall, 0_c_int, 0_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, spin_bottom_wall, 0_c_int, 1_c_int, 3_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_bound_bottom_wall, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, bound_bottom_wall, 1_c_int, 2_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_toggle_miror_bottom_wall, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, toggle_miror_bottom_wall, 1_c_int, 3_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_toggle_transparent_bottom_wall, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, toggle_transparent_bottom_wall, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_red_bottom_wall, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_green_bottom_wall, 1_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_blue_bottom_wall, 2_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, red_spin_bottom_wall, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, green_spin_bottom_wall, 1_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, blue_spin_bottom_wall, 2_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, label_refractive_index_bottom_wall, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_bottom_wall, refractive_index_spin_bottom_wall, 1_c_int, 7_c_int, 2_c_int, 1_c_int)

        expander_bottom_wall = gtk_expander_new_with_mnemonic ("bottom wall:"//c_null_char)
        CALL gtk_expander_set_child(expander_bottom_wall, table_bottom_wall)
        CALL gtk_expander_set_expanded(expander_bottom_wall, FALSE)

        CALL gtk_box_append(box, expander_bottom_wall)

    END SUBROUTINE activate_bottom_wall

END MODULE mod_bottom_wall

MODULE mod_sphere

    USE FUNCTION_gtk
    USE global_widgets

    IMPLICIT NONE

    TYPE(c_ptr) :: table_sphere, expander_sphere
    TYPE(c_ptr) :: label_sphere, label_sphere_x, label_sphere_y, label_sphere_z, label_bound_upper_sphere, &
        & label_bound_lower_sphere, label_rayon_sphere, label_toggle_miror_sphere, label_toggle_transparent_sphere, &
        & label_red_sphere, label_green_sphere, label_blue_sphere, label_refractive_index_sphere, label_ks_sphere, &
        & label_phong_sphere

    CONTAINS


    SUBROUTINE activate_sphere(box)
        
        TYPE(c_ptr), INTENT(INOUT) :: box

        !sphere
        label_sphere = gtk_label_new("sphere position"//c_null_char)

        !x
        label_sphere_x = gtk_label_new("x"//c_null_char)

        slide_sphere_x = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=0.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_sphere_x = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=0.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        !y
        label_sphere_y = gtk_label_new("y"//c_null_char)

        slide_sphere_y = hl_gtk_slider_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=0.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_sphere_y = hl_gtk_spin_button_flt_new(0._c_double, 100._c_double, 0.1_c_double, initial_VALUE=0.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        !z
        label_sphere_z = gtk_label_new("z"//c_null_char)

        slide_sphere_z = hl_gtk_slider_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=-40.d0, &
            & VALUE_changed=c_funloc(slider), length=200_c_int)
        spin_sphere_z = hl_gtk_spin_button_flt_new(-100._c_double, 100._c_double, 0.1_c_double, initial_VALUE=-40.d0, &
            & VALUE_changed=c_funloc(spinner), digits=2_c_int)

        label_bound_upper_sphere = gtk_label_new("Upper bound :"//c_null_char)
        bound_upper_sphere = hl_gtk_entry_new(VALUE="100."//c_null_char, activate=c_funloc(set_upper))

        label_bound_lower_sphere = gtk_label_new("lower bound :"//c_null_char)
        bound_lower_sphere = hl_gtk_entry_new(VALUE="-100."//c_null_char, activate=c_funloc(set_lower))

        label_rayon_sphere = gtk_label_new("radius of the sphere:"//c_null_char)
        spin_rayon_sphere = gtk_spin_button_new(gtk_adjustment_new(10d0,1d0,20d0,1d0,0d0,0d0),1d0, 0_c_int)

        label_toggle_miror_sphere = gtk_label_new("sphere is a miror ?"//c_null_char)
        toggle_miror_sphere = gtk_toggle_button_new ()

        label_toggle_transparent_sphere = gtk_label_new("sphere is a transparent ?"//c_null_char)
        toggle_transparent_sphere = gtk_toggle_button_new ()

        label_red_sphere = gtk_label_new("Red"//c_null_char)
        label_green_sphere = gtk_label_new("Green"//c_null_char)
        label_blue_sphere = gtk_label_new("Blue"//c_null_char)

        red_spin_sphere = gtk_spin_button_new(gtk_adjustment_new(1.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        green_spin_sphere = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)
        blue_spin_sphere = gtk_spin_button_new(gtk_adjustment_new(0.d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 0_c_int)

        label_refractive_index_sphere = gtk_label_new("refractive index"//c_null_char)
        refractive_index_spin_sphere = gtk_spin_button_new(gtk_adjustment_new(1.3d0,0d0,3d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        label_phong_sphere = gtk_label_new("component of phong"//c_null_char)
        phong_spin_sphere = gtk_spin_button_new(gtk_adjustment_new(1000d0,0d0,1000d0,1d0,0d0,0d0),1d0, 2_c_int)

        label_ks_sphere = gtk_label_new("component of ks"//c_null_char)
        ks_spin_sphere = gtk_spin_button_new(gtk_adjustment_new(0.3d0,0d0,1d0,0.01d0,0d0,0d0),1d0, 2_c_int)

        table_sphere = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table_sphere, TRUE)
        CALL gtk_grid_set_row_homogeneous(table_sphere, FALSE)
        CALL gtk_grid_attach(table_sphere, label_sphere, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_sphere_x, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_sphere_y, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_sphere_z, 2_c_int, 1_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, slide_sphere_x, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, slide_sphere_y, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, slide_sphere_z, 2_c_int, 2_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, spin_sphere_x, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, spin_sphere_y, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, spin_sphere_z, 2_c_int, 3_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_bound_upper_sphere, 0_c_int, 4_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, bound_upper_sphere, 1_c_int, 4_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_bound_lower_sphere, 0_c_int, 5_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, bound_lower_sphere, 1_c_int, 5_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_rayon_sphere, 0_c_int, 6_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, spin_rayon_sphere, 1_c_int, 6_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_toggle_miror_sphere, 0_c_int, 7_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, toggle_miror_sphere, 1_c_int, 7_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_toggle_transparent_sphere, 0_c_int, 8_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, toggle_transparent_sphere, 1_c_int, 8_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_red_sphere, 0_c_int, 9_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_green_sphere, 1_c_int, 9_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_blue_sphere, 2_c_int, 9_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, red_spin_sphere, 0_c_int, 10_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, green_spin_sphere, 1_c_int, 10_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, blue_spin_sphere, 2_c_int, 10_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_refractive_index_sphere, 0_c_int, 11_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, refractive_index_spin_sphere, 1_c_int, 11_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_phong_sphere, 0_c_int, 12_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, phong_spin_sphere, 1_c_int, 12_c_int, 2_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, label_ks_sphere, 0_c_int, 13_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table_sphere, ks_spin_sphere, 1_c_int, 13_c_int, 2_c_int, 1_c_int)

        expander_sphere = gtk_expander_new_with_mnemonic ("sphere:"//c_null_char)
        CALL gtk_expander_set_child(expander_sphere, table_sphere)
        CALL gtk_expander_set_expanded(expander_sphere, FALSE)

        CALL gtk_box_append(box, expander_sphere)

    END SUBROUTINE activate_sphere

END MODULE mod_sphere