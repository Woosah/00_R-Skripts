{\rtf1\ansi\ansicpg1252\cocoartf1038\cocoasubrtf360
{\fonttbl\f0\fmodern\fcharset0 Courier-Bold;\f1\fmodern\fcharset0 Courier;}
{\colortbl;\red255\green255\blue255;\red0\green48\blue246;\red53\green112\blue20;\red60\green115\blue34;
\red220\green38\blue27;}
\paperw11900\paperh16840\margl1440\margr1440\vieww19000\viewh10080\viewkind0
\deftab720
\pard\pardeftab720\ql\qnatural

\f0\b\fs24 \cf2 library
\f1\b0 \cf3 (\cf0 maps\cf3 )\cf0 \

\f0\b \cf2 library
\f1\b0 \cf3 (\cf0 hexbin\cf3 )\cf0 \

\f0\b \cf2 library
\f1\b0 \cf3 (\cf0 maptools\cf3 )\cf0 \

\f0\b \cf2 library
\f1\b0 \cf3 (\cf0 ggplot2\cf3 )\cf0 \

\f0\b \cf2 library
\f1\b0 \cf3 (\cf0 sp\cf3 )\cf0 \

\f0\b \cf2 library
\f1\b0 \cf3 (\cf0 mapproj\cf3 )\cf0 \
\'a0\
\pard\pardeftab720\ql\qnatural
\cf4 # piRate the data from the militaRy\cf0 \
\pard\pardeftab720\ql\qnatural

\f0\b \cf2 download.file
\f1\b0 \cf3 (\cf5 "http://msi.nga.mil/MSISiteContent/StaticFiles/Files/ASAM_shp.zip"\cf0 , destfile\cf3 =\cf5 "ASAM_shp.zip"\cf3 )\cf0 \

\f0\b \cf2 unzip
\f1\b0 \cf3 (\cf5 "ASAM_shp.zip"\cf3 )\cf0 \
\'a0\
\pard\pardeftab720\ql\qnatural
\cf4 # extRact the data fRame we need fRom the shape file\cf0 \
pirates.df \cf3 <-\cf0  
\f0\b \cf2 as.data.frame
\f1\b0 \cf3 (\cf0 readShapePoints\cf3 (\cf5 "ASAM 19 SEP 13"\cf3 ))\cf0  \cf4 # you may need to use a diffeRent name depending on d/l date\cf0 \
\'a0\
\cf4 # get the woRld map data\cf0 \
world \cf3 <-\cf0  map_data\cf3 (\cf5 "world"\cf3 )\cf0 \
world \cf3 <-\cf0  
\f0\b \cf2 subset
\f1\b0 \cf3 (\cf0 world, region \cf3 !=\cf0  \cf5 "Antarctica"\cf3 )\cf0  \cf4 # inteRcouRse AntaRctica\cf0 \
\'a0\
\cf4 # yeaRs we want to loop thoRugh\cf0 \
ends \cf3 <-\cf0  \cf5 1979\cf3 :\cf5 2013\cf0 \
\'a0\
\cf4 # loop thRough, extRact data, build plot, save plot: BOOM\cf0 \
\pard\pardeftab720\ql\qnatural

\f0\b \cf2 for
\f1\b0 \cf0  \cf3 (
\f0\b \cf2 end
\f1\b0 \cf0  
\f0\b \cf2 in
\f1\b0 \cf0  ends\cf3 )\cf0  \cf3 \{\cf0 \
  
\f0\b \cf2 png
\f1\b0 \cf3 (\cf0 filename\cf3 =
\f0\b \cf2 sprintf
\f1\b0 \cf3 (\cf5 "arrr-%d.png"\cf0 ,
\f0\b \cf2 end
\f1\b0 \cf3 )\cf0 ,width\cf3 =\cf5 500\cf0 ,height\cf3 =\cf5 250\cf0 ,bg\cf3 =\cf5 "white"\cf3 )\cf0  \cf4 # change to 1000x500 or laRgeR\cf0 \
  dec.df \cf3 <-\cf0  pirates.df\cf3 [\cf0 pirates.df$DateOfOcc \cf3 >\cf0  \cf5 "1970-01-01"\cf0  \cf3 &\cf0  pirates.df$DateOfOcc \cf3 <\cf0  
\f0\b \cf2 as.Date
\f1\b0 \cf3 (
\f0\b \cf2 sprintf
\f1\b0 \cf3 (\cf5 "%s-12-31"\cf0 ,
\f0\b \cf2 end
\f1\b0 \cf3 ))\cf0 ,\cf3 ]\cf0  \
  rng \cf3 <-\cf0  
\f0\b \cf2 range
\f1\b0 \cf3 (\cf0 dec.df$DateOfOcc\cf3 )\cf0 \
  p \cf3 <-\cf0  ggplot\cf3 ()\cf0  \
  p \cf3 <-\cf0  p \cf3 +\cf0  geom_polygon\cf3 (
\f0\b \cf2 data
\f1\b0 \cf3 =\cf0 world, aes\cf3 (\cf0 x\cf3 =\cf0 long, y\cf3 =\cf0 lat, group\cf3 =\cf0 group\cf3 )\cf0 , fill\cf3 =\cf5 "gray40"\cf0 , colour\cf3 =\cf5 "white"\cf3 )\cf0 \
  p \cf3 <-\cf0  p \cf3 +\cf0  stat_summary_hex\cf3 (\cf0 fun\cf3 =\cf5 "length"\cf0 , 
\f0\b \cf2 data
\f1\b0 \cf3 =\cf0 dec.df, aes\cf3 (\cf0 x\cf3 =\cf0 coords.x1, y\cf3 =\cf0 coords.x2, z\cf3 =\cf0 coords.x2\cf3 )\cf0 , alpha\cf3 =\cf5 0.8\cf3 )\cf0 \
  p \cf3 <-\cf0  p \cf3 +\cf0  scale_fill_gradient\cf3 (\cf0 low\cf3 =\cf5 "white"\cf0 , high\cf3 =\cf5 "red"\cf0 , \cf5 "Pirate Attacks recorded"\cf3 )\cf0 \
  p \cf3 <-\cf0  p \cf3 +\cf0  theme_bw\cf3 ()\cf0  \cf3 +\cf0  labs\cf3 (\cf0 x\cf3 =\cf5 ""\cf0 ,y\cf3 =\cf5 ""\cf0 , 
\f0\b \cf2 title
\f1\b0 \cf3 =
\f0\b \cf2 sprintf
\f1\b0 \cf3 (\cf5 "Pirate Attacks From %s to %s"\cf0 ,rng\cf3 [\cf5 1\cf3 ]\cf0 ,rng\cf3 [\cf5 2\cf3 ]))\cf0 \
  p \cf3 <-\cf0  p \cf3 +\cf0  theme\cf3 (\cf0 panel.background \cf3 =\cf0  element_rect\cf3 (\cf0 fill\cf3 =\cf5 '#A6BDDB'\cf0 , colour\cf3 =\cf5 'white'\cf3 ))\cf0 \
  
\f0\b \cf2 print
\f1\b0 \cf3 (\cf0 p\cf3 )\cf0 \
  
\f0\b \cf2 dev.off
\f1\b0 \cf3 ()\cf0 \
\pard\pardeftab720\ql\qnatural
\cf3 \}\cf0 \
\'a0\
\pard\pardeftab720\ql\qnatural
\cf4 # requires imagemagick\cf0 \
\pard\pardeftab720\ql\qnatural

\f0\b \cf2 system
\f1\b0 \cf3 (\cf5 "convert -delay 45 -loop 0 arrr*g arrr500.gif"\cf3 )\cf0 \
}