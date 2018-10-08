!============================================================
! ClipTojp.clw -- Example of using the SaClipboardClass to paste a DIB
!                 from the clipboard to freeimage object and save as jpeg.
!
!  Copyright © 2006 Sand & Associates, Larry@sand-associates.com
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
  Program
  Include('FreeImCl.inc'),Once
  Include('SaClipCl.inc'),Once

  Map
    INCLUDE('SaWApi.inc','Prototypes'),ONCE
  End

JpegQuality Long(75)
W    WINDOW('Clipboard to JPEG image file'),AT(,,291,130),FONT('MS Sans Serif',8,,FONT:regular),SYSTEM, |
         GRAY
       PROMPT('&JPEG Quality'),AT(27,20),USE(?JPEGQualityPrompt)
       SPIN(@n3),AT(80,18,36,12),USE(JpegQuality),LEFT(2),RANGE(0,100),STEP(5)
       BUTTON('Get Image from Clipboard and save to file'),AT(26,47,191,14),USE(?GetImageButton)
     END


fi      FreeImageClass      !need an instance of the freeimage class and
cb      SaClipboardClass    !an instance of the clipboard class

hMem UNSIGNED,Auto
  Code

  Open(W)

  Accept

    Case Field()
    Of ?GetImageButton
      If Event() = EVENT:Accepted
        !--------------------------------------------------------
        ! first try to open the clipboard
        !--------------------------------------------------------
        If cb.OpenClipboard(w) = True

          !--------------------------------------------------------
          ! If there's an image on the clipboard that the clipboard class
          ! knows how to manage it will make a copy of it and store the
          ! handle to the global memory in the hMem variable.  Otherwise
          ! hMem will equal zero on failure.
          !--------------------------------------------------------
          hMem = cb.GetClipboardCopy(SA_CF_DIB)

          If hMem <> 0

            !--------------------------------------------------------
            ! Now that you have a copy of the image from the clipboard
            ! try to load it into the FreeImage object.  Once it's in the
            ! FreeImage object you can manipulate it and save it to a file
            ! in any of the supported image formats.
            !--------------------------------------------------------
            If fi.iImage.Load(hMem, SA_GlobalSize(hMem)) = True

              !--------------------------------------------------------
              ! The image was loaded into the FreeImage object.
              ! Change the color depth to 24 bits per pixel if necessary.
              !--------------------------------------------------------
              If fi.Iimage.GetBPP() <> 24 Then
                fi.Iimage.ConvertColorDepth(FI_24BIT,,)
              End

              !--------------------------------------------------------
              ! Before saving the image you can set supported save options.
              ! The FreeImage documentation lists the options supported by
              ! each image format.  
              !--------------------------------------------------------
              fi.Iimage.SetSaveOption(JpegQuality) !jpegs support 0 to 100 quality

              !--------------------------------------------------------
              ! Finally call the SaveAs method to convert the DIB to a JPEG.
              !--------------------------------------------------------
              If fi.iImage.SaveAs('ScreenCap.jpg',FIF_JPEG) = True  
                Message('Saved the image to the file named: "'&fi.iImage.GetFilename()&'"','Clarion FreeImage')
              Else
                Message('Unable to save the image','Clarion FreeImage')
              End
            End
            SA_GlobalFree(hMem) !You must free the memory when done.
          Else
            Message('No image on clipboard','Clarion FreeImage',Icon:Exclamation)
          End
          cb.CloseClipboard()

        Else
          Message('Unable to open the clipboard','Clarion FreeImage',Icon:Exclamation)
        End !open clipboard sucessful

      End !Event
    End  !Field
  End !Accept

  Close(W)
  Return
