unit Vittix.DBGrid.Clipboard;

interface

/// <summary>
/// Clipboard access with contention tolerance. Clipboard managers, RDP
/// sessions and monitoring tools briefly hold the clipboard open, which makes
/// a single OpenClipboard attempt fail with "Access is denied". These helpers
/// retry a few times with a short pause before giving up, so exports and
/// footer copy actions stay reliable on such machines.
/// </summary>

/// <summary>Writes text to the clipboard, retrying while the clipboard is
/// held by another process. Raises the last error when all attempts fail.
/// </summary>
procedure VittixSetClipboardText(const AText: string);

/// <summary>Reads text from the clipboard with the same retry tolerance;
/// returns '' when the clipboard holds no text.</summary>
function VittixGetClipboardText: string;

implementation

uses
  System.SysUtils,
  Winapi.Windows,
  Vcl.Clipbrd;

const
  CLIPBOARD_MAX_ATTEMPTS = 5;
  CLIPBOARD_RETRY_DELAY_MS = 50;

procedure VittixSetClipboardText(const AText: string);
var
  Attempt: Integer;
begin
  for Attempt := 1 to CLIPBOARD_MAX_ATTEMPTS do
  begin
    try
      Clipboard.AsText := AText;
      Exit;
    except
      // OpenClipboard contention: pause briefly and try again, then give up
      // with the original error.
      if Attempt = CLIPBOARD_MAX_ATTEMPTS then
        raise;
    end;
    Sleep(CLIPBOARD_RETRY_DELAY_MS);
  end;
end;

function VittixGetClipboardText: string;
var
  Attempt: Integer;
begin
  Result := '';
  for Attempt := 1 to CLIPBOARD_MAX_ATTEMPTS do
  begin
    try
      Result := Clipboard.AsText;
      Exit;
    except
      if Attempt = CLIPBOARD_MAX_ATTEMPTS then
        raise;
    end;
    Sleep(CLIPBOARD_RETRY_DELAY_MS);
  end;
end;

end.
