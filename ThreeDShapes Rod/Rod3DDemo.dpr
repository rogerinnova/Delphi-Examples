program Rod3DDemo;

uses
  FMX.Forms,
  ISLibFmxUtils in 'ISLibFmxUtils.pas',
  ThreeDRod in 'ThreeDRod.pas',
  Rod3DForm in 'Rod3DForm.pas' {FormRodDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRodDemo, FormRodDemo);
  Application.CreateForm(TFormRodDemo, FormRodDemo);
  Application.Run;
end.
