import React, { FunctionComponent, useState } from "react";
import { useChat } from "../../context/chatContext";
import CreateRoomModal from "./CreateRoomModal";
import JoinModal from "./JoinModal";
import RoomCard from "./RoomCard";

const RoomList: FunctionComponent = () => {
  const chat = useChat();
  const [openJoinRoomModal, setOpenJoinRoomModal] = useState<boolean>(false);
  const [openCreateRoomModal, setOpenCreateRoomModal] = useState<boolean>(false);

  return (
    <div>

      {/* Join / create room */}
      <div>
        <button className="px-4 py-2 border-2 " onClick={()=>setOpenJoinRoomModal(true)}>
          Join room
        </button>
        <button className="px-4 py-2 border-2 " onClick={()=>setOpenCreateRoomModal(true)}>
          Create room
        </button>
      </div>

      {/* Room list */}
      {chat?.rooms?.map((room) => (
        <RoomCard 
        room={room}
        key = {room.id}
        />
      ))}

      {/* Join modal */}
      {
        openJoinRoomModal ? 
        <JoinModal setOpenJoinRoomModal={setOpenJoinRoomModal}/> : null
      }

      {/* Create room modal */}
      {
        openCreateRoomModal ?
        <CreateRoomModal setOpenCreateRoomModal={setOpenCreateRoomModal} /> : null
      }
    </div>
  );
};

export default RoomList;
