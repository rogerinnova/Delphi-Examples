program RevisedMaths3D;

uses
  System.StartUpCopy,
  FMX.Forms,
  MathsForm3D in 'MathsForm3D.pas' {MathsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMathsForm, MathsForm);
  Application.Run;
end.
