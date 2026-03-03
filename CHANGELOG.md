# Changelog

## v1.0.0 (29/Jan/2026)
- First release.

## v1.0.1 (01/Mar/2026)
- Internal changes in many subroutines, and their organization in modules.
- Deletion of unused routines.
- Addition of several internal security checks (mostly in I/O).
- Fix of makefile issues from v1.0.0.
- Fix of a bug in flg2fcs (it used to crash when a previous .map file didn't exist).
- Modifications in flg and fcs starting tools to make them accept both formatted and unformatted input files.
- Inclusion of flg2sum, fcs2sum, flg2pov, fcs2pov, flg2smb, fcs2sym, fcs2pri, and fcs2iso tools.
- In principle, this should be version 1.1, but it ended up being launched as a development intermediate for the actual version 1.1.
  
## v1.x.x (in development)
- Both flg2pri and fcs2pri tools were merged into flg2pri, which now looks first for flag-graph files and then, in their absence, looks for faces-info files.
- Both flg2sym and fcs2sym tools were merged into flg2sym, which now looks first for flag-graph files and then, in their absence, looks for faces-info files.
- Both flg2iso and fcs2iso tools were merged into flg2iso, which now looks first for flag-graph files and then, in their absence, looks for faces-info files.
- Both fcs2smb and flg2smb tools were merged into fcs2smb, which now looks first for the faces-info files and then, in their absence, looks for flag-graph files.
