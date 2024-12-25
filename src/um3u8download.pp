{*******************************************************************************
  功    能: M3U8下载
  作    者: Jesse Jin <afrusrsc@126.com>
  创建日期: 2024-12-24
*******************************************************************************}
unit uM3U8Download;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, uAES, uLogger;

type
  TDownloadEven = procedure(Sender: TObject; ATotal, AIndex: integer) of object;

  //加密模式
  TEncryptMode = (
    emNone,   //无加密
    emAES128  //AES-128
    );

  { TM3U8Download }

  TM3U8Download = class
  private
    mLog:    TLogger;
    mHttp:   TFPHTTPClient;
    mURL_Base: string;
    mList:   TStringList;
    mEncryptMode: TEncryptMode;
    mKey_AES128: TAESKey128; //TAESKey128;
    mIV:     TAESBuffer;
    mTSList: TStringList; //ts文件列表
    mConcatFile: string;  //FFmpeg的concat协议文件名
    FUseFFmpeg: boolean;
    FOnDownload: TDownloadEven;
    procedure Init;
    //提取链接基础地址
    procedure GetBaseURL(AURL: string);
    //获取密钥
    procedure GetAESKey(AURL: string);
    //获取向量
    procedure GetIV(AText: string);
    //解析密钥信息
    procedure ParseKey(AText: string);
    //获取分片列表
    procedure GetList(AURL: string);
    //获取分片内容
    procedure GetTS(AURL: string; Ams: TMemoryStream);
    //保存分片到文件
    procedure SaveTS(AFileName: string);
    //使用FFmpeg生成
    procedure SaveWithFFmpeg(AFileName: string);
    procedure SetOnDownload(AValue: TDownloadEven);
  public
    constructor Create;
    destructor Destroy; override;
    //下载
    procedure Download(AURL, AFileName: string);
    //使用FFmpeg生成
    property UseFFmpeg: boolean read FUseFFmpeg write FUseFFmpeg;
    property OnDownload: TDownloadEven read FOnDownload write SetOnDownload;
  end;

implementation

uses
  fileutil,
  uByte, process;

  { TM3U8Download }

procedure TM3U8Download.Init;
begin
  mEncryptMode := emNone;
  mConcatFile := '';
  mTSList.Clear;
  FillChar(mKey_AES128[0], Length(mKey_AES128), 0);
end;

procedure TM3U8Download.GetBaseURL(AURL: string);
var
  sl: TStringList;
  i: integer;
begin
  mURL_Base := '';
  sl := TStringList.Create;
  try
    ExtractStrings(['/'], [], PChar(AURL), sl);
    for i := 0 to sl.Count - 2 do
      mURL_Base := mURL_Base + sl[i] + '/';
    mURL_Base := StringReplace(mURL_Base, 'https:/', 'https://', [rfIgnoreCase]);
    mLog.AddDebug('基础地址: ' + mURL_Base);
  finally
    sl.Free;
  end;
end;

procedure TM3U8Download.GetAESKey(AURL: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    try
      mHttp.Get(AURL, ms);
    except
      on E: Exception do
      begin
        mLog.AddError('获取AES密钥出错', E);
        raise E;
      end;
    end;
    mLog.AddDebug('key: ' + StreamToHexStr(ms));
    ms.Position := 0;
    case mEncryptMode of
      emAES128:
        ms.Read(mKey_AES128[0], Length(mKey_AES128));
    end;
  finally
    ms.Free;
  end;
end;

procedure TM3U8Download.GetIV(AText: string);
var
  i: integer;
begin
  mLog.AddDebug('IV: ' + AText);
  for i := 0 to Length(mIV) - 1 do
  begin
    mIV[i] := StrToUInt('$' + Copy(AText, i * 2 + 1, 2)) and $FF;
  end;
end;

procedure TM3U8Download.ParseKey(AText: string);
var
  sl: TStringList;
  i: integer;
  method, url, iv: string;
begin
  url := '';
  sl := TStringList.Create;
  try
    ExtractStrings([','], [], PChar(AText), sl);
    for i := 0 to sl.Count - 1 do
    begin
      if UpperCase(sl.Names[i]) = 'METHOD' then
      begin
        method := sl.ValueFromIndex[i];
        if SameText(method, 'AES-128') then
          mEncryptMode := emAES128;
        Continue;
      end;
      if UpperCase(sl.Names[i]) = 'URI' then
      begin
        url := sl.ValueFromIndex[i];
        url := StringReplace(url, '"', '', [rfReplaceAll, rfIgnoreCase]);
        GetAESKey(url);
        Continue;
      end;
      if UpperCase(sl.Names[i]) = 'IV' then
      begin
        iv := sl.ValueFromIndex[i];
        iv := StringReplace(iv, '0x', '', [rfReplaceAll, rfIgnoreCase]);
        GetIV(iv);
        Continue;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TM3U8Download.GetList(AURL: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    try
      sl.Text := mHttp.Get(AURL);
    except
      on E: Exception do
      begin
        mLog.AddError('获取m3u8文件出错', E);
        raise E;
      end;
    end;
    mLog.AddDebug('m3u8文件: ' + sl.Text);
    mList.Clear;
    for i := 0 to sl.Count - 1 do
    begin
      //密钥
      if Pos('#EXT-X-KEY', UpperCase(sl[i])) > 0 then
      begin
        ParseKey(Copy(sl[i], Length('#EXT-X-KEY:') + 1, Length(sl[i]) -
          Length('#EXT-X-KEY:')));
        Continue;
      end;
      //分片列表
      if Pos('#EXTINF', UpperCase(sl[i])) > 0 then
      begin
        mList.Add(mURL_Base + sl[i + 1]);
        Continue;
      end;
      //结束
      if Pos('#EXT-X-ENDLIST', UpperCase(sl[i])) > 0 then
        Exit;
    end;
  finally
    sl.Free;
  end;
end;

procedure TM3U8Download.GetTS(AURL: string; Ams: TMemoryStream);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    try
      mHTTP.Get(AURL, ms);
    except
      on E: Exception do
      begin
        mLog.AddError('下载ts出错', E);
        raise E;
      end;
    end;
    ms.Position := 0;
    Ams.Clear;
    case mEncryptMode of
      emNone:
        Ams.CopyFrom(ms, ms.Size);
      emAES128:
        DecryptAESStreamCBC(ms, ms.Size, mKey_AES128, mIV, Ams);
    end;
    Ams.Position := 0;
  finally
    ms.Free;
  end;
end;

procedure TM3U8Download.SaveTS(AFileName: string);
var
  fs: TFileStream;
  ms: TMemoryStream;
  i: integer;
  Path, BaseName, TSName: string;
begin
  ms := TMemoryStream.Create;
  try
    if UseFFmpeg then
    begin
      Path := ExtractFilePath(AFileName);
      BaseName := ChangeFileExt(ExtractFileName(AFileName), '');
      Path := IncludeTrailingPathDelimiter(Path + BaseName);
      ForceDirectories(Path);
      mConcatFile := ChangeFileExt(AFileName, '.lst');
      for i := 0 to mList.Count - 1 do
      begin
        GetTS(mList[i], ms);
        TSName := Format('%s%s-%d.ts', [Path, BaseName, i + 1]);
        ms.SaveToFile(TSName);
        mTSList.Append(Format('file ''%s''', [TSName]));
        if Assigned(FOnDownload) then
          FOnDownload(Self, mList.Count, i + 1);
      end;
      mTSList.SaveToFile(mConcatFile);
    end
    else
    begin
      fs := TFileStream.Create(AFileName, fmCreate);
      try
        for i := 0 to mList.Count - 1 do
        begin
          GetTS(mList[i], ms);
          fs.CopyFrom(ms, ms.Size);
          if Assigned(FOnDownload) then
            FOnDownload(Self, mList.Count, i + 1);
        end;
      finally
        fs.Free;
      end;
    end;
  finally
    ms.Free;
  end;
end;

procedure TM3U8Download.SaveWithFFmpeg(AFileName: string);
var
  P: TProcess;
  cmd: string;
begin
  P := TProcess.Create(nil);
  try
    cmd := FindDefaultExecutablePath('ffmpeg');
    cmd := cmd + ' -f concat -safe 0 -i ' + mConcatFile + ' -c copy ' + AFileName;
    mLog.AddDebug(cmd);
    P.CommandLine := cmd;
    P.Options := P.Options + [poWaitOnExit];
    P.Execute;
  finally
    P.Free;
  end;
end;

procedure TM3U8Download.SetOnDownload(AValue: TDownloadEven);
begin
  if FOnDownload = AValue then
    Exit;
  FOnDownload := AValue;
end;

constructor TM3U8Download.Create;
begin
  mLog := LoggerMgr.GetLogger('m3u8');
  mHttp := TFPHTTPClient.Create(nil);
  //模拟Firefox进行请求
  mHttp.RequestHeaders.Values['Accept'] := '*/*';
  mHttp.RequestHeaders.Values['Accept-Encoding'] := 'gzip, deflate, br, zstd';
  mHttp.RequestHeaders.Values['Accept-Language'] :=
    'zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2';
  mHttp.RequestHeaders.Values['Connection'] := 'keep-alive';
  mHttp.RequestHeaders.Values['User-Agent'] :=
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:133.0) Gecko/20100101 Firefox/133.0';
  mList := TStringList.Create;
  mTSList := TStringList.Create;
end;

destructor TM3U8Download.Destroy;
begin
  mHTTP.Free;
  mList.Free;
  mTSList.Free;
  inherited Destroy;
end;

procedure TM3U8Download.Download(AURL, AFileName: string);
begin
  Init;
  //提取链接头
  GetBaseURL(AURL);
  //提取分片列表
  GetList(AURL);
  //下载分片
  SaveTS(AFileName);
  if UseFFmpeg then
    SaveWithFFmpeg(AFileName);
end;

end.
