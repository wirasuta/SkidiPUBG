SKIDIPUBG 2018

SYSTEM REQUIREMENTS
====================================
OS: Linux/Microsoft Windows
Hardware requirements:
- Any system powerful enough to run the OS itself.
Software requirements:
- GNU Prolog 1.4.5
(On Windows, if you wish to run this program on command prompt, make sure to set your PATH environment variable to include the gprolog installation folder.)
- Optional: GCC

HOW TO RUN?
====================================
1. On Linux, you may unzip the archive using one of the following commands:

   unzip IF2121_K03_G12.zip (to unzip to current directory)

   or, unzip -d <custom_directory> IF2121_K03_G12.zip (to unzip to any other directory)

   On Windows, you may unpack the archive using any file compression application of your choice. (WinZip, WinRAR etc.)

2. Navigate to the directory containing the source files. On both Windows and Linux you may do this by entering the following command on your terminal or command prompt:

   cd <directory_containing_source_files>

   Alternatively, on Windows you may use Windows Explorer and click your way through for this task.

3. Without GCC:
   On Windows and Linux, if you used the terminal or command prompt to complete step 2, you may enter "gprolog" (without quote marks) on the terminal or command prompt to run gprolog. Then you may type

   consult('main.pl').

   to run the program.
   
   If you used Windows Explorer on Windows to accomplish step 2, you may instead double-click on the file main.pl to run the program.

   With GCC:
   On Windows and Linux, if you completed step 2 with terminal or command prompt and have GCC installed on your machine, you may compile the code by typing in this command:

   gplc main.pl

   After that, you may run the compiled program using this command:

   ./main (on Linux)

   or, main.exe (on Windows)
