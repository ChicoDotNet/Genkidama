// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MediatorPattern {
    enum Component { Button, Panel }
    enum Event { Click, Loaded }
    enum Action { None, RefreshPanel, EnableButton }
    function notify(Component sender, Event event_) private pure returns (Action) {
        if (sender == Component.Button && event_ == Event.Click) return Action.RefreshPanel;
        if (sender == Component.Panel && event_ == Event.Loaded) return Action.EnableButton;
        return Action.None;
    }
    function run() internal pure returns (bool) {
        return notify(Component.Button, Event.Click) == Action.RefreshPanel
            && notify(Component.Panel, Event.Loaded) == Action.EnableButton;
    }
}
