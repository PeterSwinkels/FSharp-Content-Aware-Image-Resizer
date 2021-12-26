//The namespaces used by this module.
open System
open System.Collections.Generic
open System.Diagnostics
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Linq
open System.Reflection
open System.Runtime.InteropServices

//This structure defines a seam.
[<NoComparisonAttribute>]
type SeamStr =
   {
      Energy : int          //Contains the seam's energy.
      Indexes : list<int>   //Contains the indexes of the seam's pixels.
   }

let ARGBSize = 4                                                                                  //The number of bytes in an alpha, red, green, and blue color value.
let ProgramInformation = FileVersionInfo.GetVersionInfo(Assembly.GetExecutingAssembly().Location) //Contains this program's information.

//This function returns the difference between the two specified colors.
let ColorDifference color1 color2 =
   let rec ParseRGB color1 color2 difference = 
      if color1 = [] || color2 = [] then
         difference / 3
      else
         ParseRGB color1.Tail color2.Tail (difference + Math.Abs((color2.Head |> int) - (color1.Head |> int)))
   ParseRGB color1 color2 0

//This function returns the index of the lowest difference.
let GetLowest differences =
   let rec ParseDifferences differences lowest lowestIndex index =
       if differences = [] then
          lowestIndex
       else
          ParseDifferences differences.Tail (if differences.Head <= lowest then differences.Head else lowest) (if differences.Head <= lowest then index else lowestIndex) (index + 1)
      
   ParseDifferences differences Int32.MaxValue 0 0

//This function returns a vertical seam through the specified locked bitmap.
let VerticalSeam (pixels:List<Byte>) stride height startX = 
   let RGB index = [pixels.[index]; pixels.[index + 1]; pixels.[index + 2]]

   let rec ParsePixels (pixels:List<Byte>) stride height seam x y = 
      if y >= height then
         seam
      else
         let CurrentIndex = (y * stride) + x
         let Differences = if x >= ARGBSize then [ColorDifference (RGB CurrentIndex) (RGB ((y * stride) + (x - ARGBSize)))] else [Byte.MaxValue |> int]
                           @  if y < height - 1 then [ColorDifference (RGB CurrentIndex) (RGB (((y + 1) * stride) + x))] else [Byte.MaxValue |> int]
                           @  if x + ARGBSize < stride then [ColorDifference (RGB CurrentIndex) (RGB ((y * stride) + (x + ARGBSize)))] else [Byte.MaxValue |> int]      
         let Lowest = GetLowest Differences
         ParsePixels (pixels:List<Byte>) stride height ({Energy = seam.Energy + Differences.[Lowest]; Indexes = seam.Indexes @ [CurrentIndex]}) (x + match Lowest with | 0 -> -ARGBSize | 2 -> +ARGBSize | _ -> 0) (y + 1)      
   ParsePixels (pixels:List<Byte>) stride height ({Energy = 0; Indexes = []}) startX 0

//This function returns the best vertical seam.
let BestVerticalSeam pixels stride height =   
   let rec ParsePixels pixels stride height bestSeam x = 
      if bestSeam.Energy = 0 || x >= stride then
         bestSeam.Indexes
      else
         let Seam = VerticalSeam pixels stride height x
         ParsePixels pixels stride height (if Seam.Energy < bestSeam.Energy then Seam else bestSeam) (x + ARGBSize)
   ParsePixels pixels stride height (VerticalSeam pixels stride height 0) ARGBSize

//This function resizes the specified image.
let ResizeImage (imageToResize:Bitmap) requestedResizes vertical =
   imageToResize.RotateFlip(if vertical then RotateFlipType.Rotate90FlipXY else RotateFlipType.RotateNoneFlipNone)

   let Resizes = if imageToResize.Width + requestedResizes < 1 then -(imageToResize.Width - 1) else requestedResizes
   let BitmapDataO = imageToResize.LockBits(new Rectangle(0, 0, imageToResize.Width, imageToResize.Height), ImageLockMode.ReadWrite, PixelFormat.Format32bppPArgb)
   let Buffer = Array.create (BitmapDataO.Stride * imageToResize.Height) (new Byte())      
   Marshal.Copy(BitmapDataO.Scan0, Buffer, Buffer.GetLowerBound(0), Buffer.GetUpperBound(0))
   imageToResize.UnlockBits(BitmapDataO)
   let ImagePixels = Buffer.ToList()

   let rec DoResizes resize newStride = 
      if resize >= Math.Abs(Resizes) then
         newStride
      else
         let BestSeam = BestVerticalSeam ImagePixels newStride imageToResize.Height
         List.sort BestSeam |> List.rev |> List.iter(fun index -> if Resizes > 0 then
                                                                     ImagePixels.InsertRange(index + ARGBSize, ImagePixels.GetRange(index, ARGBSize))
                                                                  else
                                                                     ImagePixels.RemoveRange(index, ARGBSize))
         DoResizes (resize + 1) (newStride + if Resizes > 0 then +ARGBSize else -ARGBSize)
   
   let ResizedImage = new Bitmap((DoResizes 1  BitmapDataO.Stride) / ARGBSize, imageToResize.Height)
   let NewBitmapDataO = ResizedImage.LockBits(new Rectangle(0, 0, ResizedImage.Width, ResizedImage.Height), ImageLockMode.ReadWrite, PixelFormat.Format32bppPArgb)
   Marshal.Copy((ImagePixels.ToArray()), (ImagePixels.ToArray()).GetLowerBound(0), NewBitmapDataO.Scan0, (ImagePixels.ToArray()).GetUpperBound(0))
   ResizedImage.UnlockBits(NewBitmapDataO)
   
   ResizedImage.RotateFlip(if vertical then RotateFlipType.Rotate270FlipXY else RotateFlipType.RotateNoneFlipNone)

   ResizedImage

//This function is executed when this program is started.
[<EntryPoint>]
let main argv = 
   if argv.Count() = 0 then
      printfn "%s" ("Usage: \"" + Path.GetFileName(ProgramInformation.FileName) + "\" Source Target Resizes Vertical\n\
                     \tSource = Any supported image file.\n\
                     \tTarget = Path to save image to. This will always be *.png file.\n\
                     \tResizes = Number of columns/rows to add/remove. Negative = remove; positive = add.\n\
                     \tVertical = 0 = horizontal; 1 = vertical
                     \n\
                     Return values: -1 = this help was displayed\n\
                     \t\t1 = an image was successfully resized.")
      -1
   else
      (ResizeImage (new Bitmap(argv.[0])) (argv.[2] |> int) ((argv.[3] |> int) = 1)).Save(argv.[1] + ".png", ImageFormat.Png)      
      Process.Start(new ProcessStartInfo(FileName = argv.[1] + ".png", UseShellExecute = true)) |> ignore
      1

     