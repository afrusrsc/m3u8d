program m3u8d;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  FM_Main,
  uM3U8Download;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.TaskBarBehavior := tbSingleButton;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
