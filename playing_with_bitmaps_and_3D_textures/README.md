# Playing with Colors Bitmaps and Textures in Delphi

I have been exploring Textures in Delphi 3D and this reqired an understanding of colors and at a later time will require the contruction of very specific Bitmaps modeled on the one used by Anders Ohisson in his Maths Function Demo I palyed with
https://github.com/rogerinnova/Delphi-Examples/tree/master/visualizing_math_functions_by_generating_firemonkey_meshes

This folder contains two simple demo projects and a library of functions for creating bitmaps and textures in Delphi for Delphi.


##The "Library" File UsefulBmpConcepts.pas 

*This file contains a number of function to create auto generated bitmaps most of which are plays arround the logic of Anders' 360 x 1 bitmap showing the rainbow of colors used in his Maths 3D Functions project but which enables grid lines to be added and allow you to see the effects of changing the saturation, hue, Lumin and opicity.

One function "LandscapeBitMap" starts to look at producing the special effect bitmap I need for my future project.

*The file also generates and applies Material sources of various complexions to 3D components derived from TCustomMesh. These default creations are useful when you want to programmaticcally play with Material sources.

##The Project BitmapBuildExamples
A single form (BitmapForm.pas) with multiple image components which are loaded programatically when Timer1 fires. It uses UsefulBmpConcepts.pas to add Bitmap Images to Firemonkey Timages and TImageControls. The Bitmaps generated have graded  Hue (H), Saturation (S), Luminance (L) and Opacity to demonstrate and easily play with these effects. To show the effect of Image Opacity the background behind these controls have labeled text.

##The Project Bitmap3DExamples
A single form project (BitMap3DForm) which looks at the use of Texture, Opicity and TwoSided on the 3D shapes components on the form.