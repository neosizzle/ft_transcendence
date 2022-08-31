import React, { FunctionComponent, useState } from "react";
import { User } from "../../context/authContext";
import { ControlledMenu, MenuItem, useMenuState } from "@szhsin/react-menu";
import "@szhsin/react-menu/dist/index.css";
import { useChat } from "../../context/chatContext";

interface MemberCardProps {
  user: User;
}

const MemberCard: FunctionComponent<MemberCardProps> = ({ user }) => {
  const [menuProps, toggleMenu] = useMenuState();
  const [anchorPoint, setAnchorPoint] = useState({ x: 0, y: 0 });
  const chat = useChat()

  return (
    <div
      onContextMenu={(e) => {
		if (chat?.activeRoom?.type === "DM") return;
        e.preventDefault();
        setAnchorPoint({ x: e.clientX, y: e.clientY });
        toggleMenu(true);
      }}
      key={user.id}
      className="border-2 text-center text-xl"
    >
      {`${user.username}`}
      <ControlledMenu
        {...menuProps}
        anchorPoint={anchorPoint}
        onClose={() => toggleMenu(false)}
      >
        <MenuItem>{user.username}</MenuItem>
      </ControlledMenu>
    </div>
  );
};

export default MemberCard;
