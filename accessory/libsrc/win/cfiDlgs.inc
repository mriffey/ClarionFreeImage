!ABCIncludeFile(FREEIMAGE)
   OMIT('_EndOfCfiDlgsClass_',_CfiDlgsClassIncluded_)
_CfiDlgsClassIncluded_  EQUATE(1)
!============================================================
! cfiDlgs.inc -- Clarion FreeImage Dialogs header file
!
!  Copyright © 2005 -2006 Sand & Associates, Larry@sand-associates.com
!
!  This software is provided 'as-is', without any express or implied
!  warranty.  In no event will the authors be held liable for any damages
!  arising from the use of this software.
!
!  Permission is granted to anyone to use this software for any purpose,
!  including commercial applications, and to alter it and redistribute it
!  freely, subject to the following restrictions:
!
!  1. The origin of this software must not be misrepresented; you must not
!     claim that you wrote the original software. If you use this software
!     in a product, an acknowledgment in the product documentation would be
!     appreciated but is not required.
!
!  2. Altered source versions must be plainly marked as such, and must not be
!     misrepresented as being the original software.
!
!  3. This notice may not be removed or altered from any source distribution.
!
!============================================================
  Include('cfiImgCt.inc'),Once

EVENT:UpdatPreviewImage EQUATE(EVENT:User+197)
ROTATECLOCKWISE Equate(1)
ROTATECCW       Equate(2)

cfiDialogsClass  Class,Type,|
                      Module('cfiDlgs.clw'), |
                      Link('cfiDlgs.clw',_FreeImageLinkMode_), DLL(_FreeImageDLLMode_)

IC  &cfiImageControl

iImgCtrl    &iImageControl
iImg        &iImage

Init                        Procedure(*iImageControl iImgCtrl, *iImage iImg)
BrightnessContrastDialog    Procedure(<String sTitle>),BOOL,Proc,Virtual
GammaDialog                 Procedure(<String sTitle>),BOOL,Proc,Virtual
RescaleDialog               Procedure(<String sTitle>),BOOL,Proc,Virtual
RotateCenterDialog          Procedure(<String sTitle>),Bool,Proc,Virtual
RotateSkewDialog            Procedure(<String sTitle>),Bool,Proc,Virtual
MetadataDialog              Procedure(<String sTitle>),Bool,Proc,Virtual


                 End
!_EndOfCfiDlgsClass_