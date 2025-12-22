# FEV-tools
![logo](Doc/figs/fevtools.png)
Fortran-written collection of tools to work with embedding tensors representing 3-regular graphs (analogous to 2D nanocarbon structures made solely of tricoordinated atoms). 

It deals with a simple .fev format for the tensor and with the related .flg/.fcs formats associated with more complex entities describing the 3-regular graphs (flag graph/faces info set).

In addition to simple conversions for the .fev, .flg, and .fcs formats, it runs
- symmetry check
- isomorphism check
- tensor illustration 
- structure generation 
- coordinates generation 
- reduction to primitive cell
- polygonal symbol generation 
- sum rules computation 
- binary from/to formatted conversion 

The package consists of a Src folder with the source code files and a makefile. It also includes a Doc folder with the user guide.

