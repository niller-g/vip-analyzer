% Copyright

implement aboutDialog inherits dialog
    open vpiDomains

clauses
    display(Parent) = Dialog :-
        Dialog = new(Parent),
        Dialog:show().

constants
    versionString1 = "{FileDescription}".
    versionString2 = "Version {FileVersionA}.{FileVersionB}\n{LegalCopyright}\n{CompanyName}".

constructors
    new : (window Parent).
clauses
    new(Parent) :-
        dialog::new(Parent),
        generatedInitialize(),
        version1_ctl:setText(versionString1),
        version2_ctl:setText(versionString2),
        addShowListener(
            {  :-
                gui_native::logfont(:Height = Height | Font) = version1_ctl:getFont(),
                version1_ctl:setFont(gui_native::logfont(:Height = Height * 3 div 2, :Weight = 800 | Font))
            }).

% This code is maintained automatically, do not update it manually.
facts
    version1_ctl : versionControl.
    version2_ctl : versionControl.

predicates
    generatedInitialize : ().
clauses
    generatedInitialize() :-
        setText("About"),
        setRect(rct(122, 26, 386, 214)),
        setModal(true),
        setDecoration(titlebar([frameDecoration::closeButton])),
        setState([wsf_NoClipSiblings]),
        Icon1_ctl = iconControl::new(This),
        Icon1_ctl:setIcon(application_icon),
        Icon1_ctl:setPosition(16, 12),
        version1_ctl := versionControl::new(This),
        version1_ctl:setPosition(16, 42),
        version1_ctl:setSize(232, 20),
        GroupBox_ctl = groupBox::new(This),
        GroupBox_ctl:setText(""),
        GroupBox_ctl:setPosition(8, 76),
        GroupBox_ctl:setSize(248, 56),
        version2_ctl := versionControl::new(GroupBox_ctl),
        version2_ctl:setPosition(7, 2),
        version2_ctl:setSize(232, 38),
        version2_ctl:setTabStop(true),
        version2_ctl:setAlignBaseline(false),
        ButtonOk = button::newCancel(This),
        ButtonOk:setText("&OK"),
        ButtonOk:setPosition(100, 152),
        ButtonOk:setWidth(56),
        ButtonOk:defaultHeight := true.
% end of automatic code

end implement aboutDialog
