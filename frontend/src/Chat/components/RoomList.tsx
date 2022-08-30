import React, { FunctionComponent, useState } from "react";
import { useChat } from "../../context/chatContext";
import JoinModal from "./JoinModal";

const RoomList: FunctionComponent = () => {
  const chat = useChat();
  const [openJoinRoomModal, setOpenJoinRoomModal] = useState<boolean>(false);

  return (
    <div>

      {/* Join / create room */}
      <div>
        <button className="px-4 py-2 border-2 " onClick={()=>setOpenJoinRoomModal(true)}>
          Join room
        </button>
        <button className="px-4 py-2 border-2 ">
          Create room
        </button>
      </div>

      {/* Room list */}
      {chat?.rooms?.map((room) => (
        <div
          className="text-xl border-2 cursor-pointer"
          onClick={() => {
            chat.setActiveRoom(room);
            console.log("setted active room romlsit ", room.id)
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

      {/* Join modal */}
      {
        openJoinRoomModal ? 
        <JoinModal setOpenJoinRoomModal={setOpenJoinRoomModal}/> : null
      }
    </div>
  );
};

export default RoomList;
