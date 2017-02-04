unit BitmapForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  UsefulBmpConcepts, FMX.Objects, FMX.Controls.Presentation;

type
  TFormTestBitmap = class(TForm)
    ImageControl1: TImageControl;
    Timer1: TTimer;
    ImageControl2: TImageControl;
    Image1: TImage;
    ImageControl3: TImageControl;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    ImageControl4: TImageControl;
    Label1: TLabel;
    Label2: TLabel;
    StyleBook1: TStyleBook;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestBitmap: TFormTestBitmap;

implementation

{$R *.fmx}

procedure TFormTestBitmap.Timer1Timer(Sender: TObject);
Var
  Bmp:TBitMap;
begin
  Bmp:= ColorBitMap(100,360,0.750,0.5,true,true);
  try
  ImageControl1.Bitmap:= Bmp;
  Image1.Bitmap:=Bmp;
  finally
    Bmp.Free;
  end;
  Bmp:= ColorLumBitMap(400,360,0.750);
  try
  ImageControl2.Bitmap:= Bmp;
  Image2.Bitmap:=Bmp;
  finally
    Bmp.Free;
  end;
  Bmp:= ColorSatBitMap(400,360,0.50);
  try
  ImageControl3.Bitmap:= Bmp;
  Image3.Bitmap:=Bmp;
  finally
    Bmp.Free;
  end;
  Bmp:= ColorTransparentBitMap(400,360,0.75,0.50);
  try
  ImageControl4.Bitmap:= Bmp;
  Image4.Bitmap:=Bmp;
  finally
    Bmp.Free;
  end;


  Bmp:= LandscapeBitMap(100,360,0,0.75,0.50,1.0);
  try
  Image5.Bitmap:= Bmp;
  finally
    Bmp.Free;
  end;

  Bmp:= LandscapeBitMap(100,360,2,0.75,0.50,1.0);
  try
  Image6.Bitmap:= Bmp;
  finally
    Bmp.Free;
  end;

  Bmp:= LandscapeBitMap(100,360,3,0.75,0.50,1.0);
  try
  Image7.Bitmap:= Bmp;
  finally
    Bmp.Free;
  end;

  Bmp:= LandscapeBitMap(100,360,4,0.75,0.50,1.0);
  try
  Image8.Bitmap:= Bmp;
  finally
    Bmp.Free;
  end;




end;

end.
