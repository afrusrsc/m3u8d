unit FM_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, EditBtn, uM3U8Download;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRun: TButton;
    ccbUseFFmpeg: TCheckBox;
    ccbDebug: TCheckBox;
    edtURL: TLabeledEdit;
    edtFile: TFileNameEdit;
    lblFile: TLabel;
    lblProgress: TLabel;
    mmoLog: TMemo;
    pb1: TProgressBar;
    procedure btnRunClick(Sender: TObject);
    procedure ccbDebugChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    m3u8: TM3U8Download;
    procedure ProcDownload(Sender: TObject; ATotal, AIndex: integer);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  uMsg, uLogger;

const //使用说明
  Usage = '【使用说明】' + #13#10 //
    + '1. 用【火狐】打开网页，找到想要下载的视频，并播放' +
    #13#10 //
    + '2. 按【F12】进入到调试模式' + #13#10 //
    + '3. 从出来的界面中点【网络】' + #13#10 //
    + '4. 在下方选择【XHR】' + #13#10 //
    + '5. 按【F5】刷新，或者点界面中的【重新载入】' + #13#10 //
    + '6. 选择第一条【方法】列为【GET】，且【类型】列为【vnd.apple.mpegurl】'
    + #13#10 //
    + '7. 复制【GET】后面的网址，例如：【https://mov.bn.netease.com/open-movie/nos/m3u8/2023/12/28/SIM40KTM5_shd.m3u8】'
    + #13#10 //
    + '8. 把网址粘贴到本工具的【下载地址】里' + #13#10 //
    + '9. 点【选择位置】，选择一个要保存的文件夹，并给要下载的视频起个名字'
    + #13#10 //
    + '10.点【开始下载】等待视频下载完成' + #13#10 //
    + '11.重复以上操作下载其它视频' + #13#10 //
    + '--------------------------------------------------------------------------------'
    + #13#10 //
    + '【注意事项】' + #13#10 //
    + '1. 勾选【使用FFmpeg生成】会将临时文件缓存到同名文件夹下，生成后需要手动删除'
    + #13#10 //
    + '2. 由于网上视频编码格式的多样性，并非所有下载视频均可播放' + #13#10 //
    + '================================================================================';

  { TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoggerMgr.Compress := True;
  LoggerMgr.LogLevel := LOGLEVEL_ERROR;
  m3u8 := TM3U8Download.Create;
  m3u8.OnDownload := @ProcDownload;
  mmoLog.Lines.Add(Usage);
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
begin
  if Trim(edtURL.Text) = '' then
  begin
    ShowError('下载地址不能为空！');
    Exit;
  end;
  if Trim(edtFile.Text) = '' then
  begin
    ShowError('保存位置不能为空！');
    Exit;
  end;
  mmoLog.Lines.Add(Format('[%s]从【%s】下载到【%s】',
    [FormatDateTime('HH:NN:SS', Now), edtURL.Text, edtFile.Text]));
  m3u8.UseFFmpeg := ccbUseFFmpeg.Checked;
  m3u8.Download(edtURL.Text, edtFile.Text);
  mmoLog.Lines.Add(Format('[%s]%s', [FormatDateTime('HH:NN:SS', Now), 'OK!']));
end;

procedure TfrmMain.ccbDebugChange(Sender: TObject);
begin
  if ccbDebug.Checked then
    LoggerMgr.LogLevel := LOGLEVEL_DEBUG
  else
    LoggerMgr.LogLevel := LOGLEVEL_ERROR;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  m3u8.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  lblProgress.Left := btnRun.Left;
end;

procedure TfrmMain.ProcDownload(Sender: TObject; ATotal, AIndex: integer);
begin
  pb1.Max := ATotal;
  pb1.Position := AIndex;
  lblProgress.Caption := Format('%d/%d', [AIndex, ATotal]);
  Self.Update;
end;

end.
