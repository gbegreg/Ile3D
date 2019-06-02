program demo5_bonus2;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  principale in 'principale.pas' {fPrincipale};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfPrincipale, fPrincipale);
  Application.Run;
end.
