MODULE handlers

    USE mod_left_wall
    USE mod_scene
    USE mod_right_wall
    USE mod_top_wall
    USE mod_below_wall
    USE mod_bottom_wall
    USE mod_sphere

    USE all_MODULE_gtk

    IMPLICIT NONE

    TYPE(c_ptr) :: window
    INTEGER(c_int) :: run_status = TRUE
    INTEGER(c_int) :: width, height, firstTab, secondTab
    TYPE(c_ptr) :: box, scwin, table, box_choice, table_choice, box_area, scwin_drawing
    TYPE(c_ptr) :: button1, button2, button3, button4
    TYPE(c_ptr) :: notebook, notebookLabel, textView, scrolled_window, notebookLabel2, notebookLabe3

    
    CONTAINS

    SUBROUTINE destroy_window(widget, event, gdata) BIND(c)

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: widget, event, gdata
    
        PRINT*, "Your close window !"
        run_status = FALSE
        CALL gtk_window_destroy(window)

    END SUBROUTINE destroy_window

    SUBROUTINE my_draw_FUNCTION(widget, my_cairo_context, width, height, gdata) BIND(c)

        USE cairo, ONLY: cairo_paint
        USE gdk, ONLY: gdk_cairo_set_source_pixbuf

        IMPLICIT NONE
    
        TYPE(c_ptr), VALUE, INTENT(IN)    :: widget, my_cairo_context, gdata
        INTEGER(c_int), VALUE, INTENT(IN) :: width, height
    
        ! We redraw the pixbuf:
        CALL gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
        CALL cairo_paint(my_cairo_context)
      END SUBROUTINE my_draw_FUNCTION

    SUBROUTINE activate(app, gdata) BIND(c)

        USE FUNCTION_gtk

        IMPLICIT NONE

        TYPE(c_ptr), VALUE, INTENT(IN) :: app, gdata
        INTEGER :: i

        window = gtk_application_window_new(app)

        CALL gtk_window_set_title(window, " raytacing fortran "//c_null_char)
        width  = 700
        height = 700
        CALL gtk_window_set_default_size(window, width, height)

        box_choice = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)

        box_area = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)

        box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)

        button1 = gtk_button_new_with_mnemonic ("_ray tracing"//c_null_char)
        CALL g_signal_connect (button1, "clicked"//c_null_char, c_funloc(firstbutton))
        button2 = gtk_button_new_with_mnemonic ("_path tracing"//c_null_char)
        CALL g_signal_connect (button2, "clicked"//c_null_char, c_funloc(secondbutton))
        button3 = gtk_button_new_with_mnemonic ("_Save as PNG"//c_null_char)
        CALL g_signal_connect (button3, "clicked"//c_null_char, c_funloc(thirdbutton))
        button4 = gtk_button_new_with_mnemonic ("_Exit"//c_null_char)
        CALL g_signal_connect (button4, "clicked"//c_null_char, c_funloc(destroy_window))

        !scene
        CALL activate_scene(box)

        !left wall
        CALL activate_left_wall(box)

        !right wall
        CALL activate_right_wall(box)

        !top wall
        CALL activate_top_wall(box)

        !below wall
        CALL activate_below_wall(box)

        !bottom wall
        CALL activate_bottom_wall(box)

        !sphere
        CALL activate_sphere(box)


        scwin = gtk_scrolled_window_new()
        CALL gtk_scrolled_window_set_child(scwin, box)
        CALL gtk_scrolled_window_set_policy(scwin, GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS)
        CALL gtk_scrolled_window_set_placement (scwin, GTK_CORNER_TOP_RIGHT)

        my_drawing_area = gtk_drawing_area_new()
        CALL gtk_drawing_area_set_content_width(my_drawing_area, pixwidth)
        CALL gtk_drawing_area_set_content_height(my_drawing_area, pixheight)
        CALL gtk_drawing_area_set_draw_func(my_drawing_area, &
            & c_funloc(my_draw_FUNCTION), c_null_ptr, c_null_funptr)

        scwin_drawing = gtk_scrolled_window_new()
        CALL gtk_scrolled_window_set_child(scwin_drawing, my_drawing_area)
        CALL gtk_scrolled_window_set_policy(scwin_drawing, GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS)
        CALL gtk_scrolled_window_set_placement (scwin_drawing, GTK_CORNER_TOP_RIGHT)

        notebook = gtk_notebook_new ()
        CALL gtk_widget_set_vexpand (notebook, TRUE)
        notebookLabel = gtk_label_new_with_mnemonic("_Graphics"//c_null_char)
        firstTab = gtk_notebook_append_page (notebook, scwin_drawing, notebookLabel)

        textView = gtk_text_view_new ()
        buffer = gtk_text_view_get_buffer (textView)
        CALL gtk_text_buffer_set_text (buffer, "raytacing in fortran"//C_NEW_LINE//c_null_char,-1_c_int)
        scrolled_window = gtk_scrolled_window_new()
        notebookLabel2 = gtk_label_new_with_mnemonic("_Messages"//c_null_char)
        CALL gtk_scrolled_window_set_child(scrolled_window, textView)
        secondTab = gtk_notebook_append_page (notebook, scrolled_window, notebookLabel2)


        notebookLabe3 = gtk_label_new_with_mnemonic("_set up"//c_null_char)
        firstTab = gtk_notebook_append_page (notebook, scwin, notebookLabe3)

        table = gtk_grid_new ()
        CALL gtk_grid_set_column_homogeneous(table, TRUE)
        CALL gtk_grid_set_row_homogeneous(table, FALSE)
        CALL gtk_grid_attach(table, button1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table, button2, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table, button3, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table, button4, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
        CALL gtk_grid_attach(table, notebook, 0_c_int, 1_c_int, 4_c_int, 1_c_int)



        CALL gtk_window_set_child(window, table)
        CALL gtk_window_set_mnemonics_visible (window, TRUE)

        my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, pixwidth, pixheight)
        nch = gdk_pixbuf_get_n_channels(my_pixbuf)
        rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
        CALL c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, [pixwidth*pixheight*nch])
        pixel = CHAR(0)

        CALL gtk_widget_show(window)

    END SUBROUTINE activate


END MODULE handlers