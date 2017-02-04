program BitmapBuildExamples;

uses
  System.StartUpCopy,
  FMX.Forms,
  BitmapForm in 'BitmapForm.pas' {FormTestBitmap},
  UsefulBmpConcepts in 'UsefulBmpConcepts.pas';

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown:=true;
  Application.Initialize;
  Application.CreateForm(TFormTestBitmap, FormTestBitmap);
  Application.Run;
end.
