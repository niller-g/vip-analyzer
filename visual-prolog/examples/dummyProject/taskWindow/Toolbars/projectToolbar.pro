% Copyright

implement projectToolbar
    open vpiDomains, vpiToolbar, resourceIdentifiers

clauses
    create(Parent) :-
        StatusBar = statusBar::newApplicationWindow(),
        StatusCell = statusBarCell::new(StatusBar, 0),
        StatusBar:cells := [StatusCell],
        Toolbar = create(style, Parent, controlList),
        setStatusHandler(Toolbar, { (Text) :- StatusCell:text := Text }).

% This code is maintained automatically, do not update it manually. 16:42:04-24.4.2013
constants
    style : vpiToolbar::style = tb_top.
    controlList : control* =
        [
            tb_ctrl(id_file_new, pushb, resId(idb_NewFileBitmap), "New;New File", 1, 1),
            tb_ctrl(id_file_open, pushb, resId(idb_OpenFileBitmap), "Open;Open File", 1, 1),
            tb_ctrl(id_file_save, pushb, resId(idb_SaveFileBitmap), "Save;Save File", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_edit_undo, pushb, resId(idb_UndoBitmap), "Undo;Undo", 1, 1),
            tb_ctrl(id_edit_redo, pushb, resId(idb_RedoBitmap), "Redo;Redo", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_edit_cut, pushb, resId(idb_CutBitmap), "Cut;Cut to Clipboard", 1, 1),
            tb_ctrl(id_edit_copy, pushb, resId(idb_CopyBitmap), "Copy;Copy to Clipboard", 1, 1),
            tb_ctrl(id_edit_paste, pushb, resId(idb_PasteBitmap), "Paste;Paste from Clipboard", 1, 1),
            vpiToolbar::separator,
            tb_ctrl(id_help_contents, pushb, resId(idb_HelpBitmap), "Help;Help", 1, 1)
        ].
% end of automatic code

end implement projectToolbar
