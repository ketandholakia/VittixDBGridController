unit Vittix.DBGrid.Paging.GlobalScroll;

interface

type
  /// <summary>Implemented by datasets whose RecordCount and RecNo describe
  /// the full source row set while only one page is held in memory. The
  /// grid uses this interface to route the navigation paths that call the
  /// non-virtual TDataSet.First/Last (scrollbar endpoints and Ctrl+Home /
  /// Ctrl+End) through global seeks instead. Thumb and keyboard scrolling
  /// need no interception: they read RecordCount/RecNo and assign RecNo,
  /// which the dataset overrides directly.</summary>
  IVittixGlobalPageScroll = interface
    ['{4E6C8F3A-9B1D-4A57-B2C6-3D08A5F17E42}']
    /// <summary>True while a total row count is known and the virtual
    /// RecordCount/RecNo semantics are active.</summary>
    function GlobalScrollActive: Boolean;
    /// <summary>Total source row count the global scrollbar represents.
    /// Meaningful only while GlobalScrollActive is True.</summary>
    function GlobalRecordCount: Integer;
    /// <summary>Positions on the first global row, loading page zero.</summary>
    procedure GlobalFirst;
    /// <summary>Positions on the last global row, loading the last page.</summary>
    procedure GlobalLast;
  end;

implementation

end.
