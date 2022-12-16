# raytracing fortran

<br/>

This Project contains source code for a ray-tracing simulation on a sphere in a scene.
I realized this project under the Fortran 90 programming language and with the GTK libraries (gtk fortran)

<br/>

# Video

I realized this project from the lessons of Nicolas Bonneel : https://www.youtube.com/watch?v=1HYhrx9bzP8

<br/>

# Features

* A menu to modify the parameters of the scene and the sphere

* It can simulate with direct lighting :

<p align="center">
	<img src="image_save/image_raytracing_normal_ray.png" width="500">
</p>

* It can also simulate with indirect lighting (as well other functionality) :

<p align="center">
	<img src="image_save/image_raytracing_normal_path.png" width="500">
</p>

* It can also simulate a mirror sphere :

<p align="center">
    <img src="image_save/image_raytracing_miror_ray.png" width="250">
    <img src="image_save/image_raytracing_miror_path.png" width="250">
</p>

* As well as a transparent sphere :

<p align="center">
    <img src="image_save/image_raytracing_transparente_ray.png" width="250">
    <img src="image_save/image_raytracing_transparente_path.png" width="250">
</p>

# Dependecies

You will also need the [**GTK**](https://www.gtk.org/) libraries

As well as Fortran Package Manager [**fpm**](https://fpm.fortran-lang.org/)

# Launch

Have the gfortran compiler

</br>

In a terminal write <span style="color: #2980B9 ">fpm run --flag -fopenmp</span>

# Credits

* [**Minard Jules**](https://github.com/Minard-Jules) : Creator of the project.# raytracing_fortran
