import React, { FunctionComponent } from "react";
import { useChat } from "../../context/chatContext";

const RoomList: FunctionComponent = () => {
  const chat = useChat();

  return (
    <div>
      {chat?.rooms?.map((room) => (
        <div
          className="text-xl border-2 cursor-pointer"
          onClick={() => {
            chat.setActiveRoom(room);
          }}
          key={room.id}
        >
          {
            room.type === "DM" ? (
              <img src="/assets/default-pp.webp" width="100" height="100" />
            ) : (
              <img
                src="/assets/default-gc-harambe.png"
                width="100"
                height="100"
              />
            ) // Can't pass in value to src for some reason?
          }
          {
            room.type === "DM"
              ? "Is a DM (Should display other user's name)"
              : room.roomName // But how to get specific room's users
          }
        </div>
      ))}
    </div>
  );
};

export default RoomList;
