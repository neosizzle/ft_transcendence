import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_get } from "../../utils";
import { Member, Room } from "../classes";
import CreateRoomModal from "./CreateRoomModal";
import JoinModal from "./JoinModal";
import RoomCard from "./RoomCard";

const memberEndpoint = `${API_ROOT}/members`;
const ROOM_PAGE_SIZE = 5;

const RoomList: FunctionComponent = () => {
  const chat = useChat();
  const auth = useAuth();
  const navigate = useNavigate();
  const [openJoinRoomModal, setOpenJoinRoomModal] = useState<boolean>(false);
  const [openCreateRoomModal, setOpenCreateRoomModal] = useState<boolean>(false);
  const [currChatPage, setCurrChatPage] = useState<number>(1);
  const [hitBtm, setHitBtm] = useState<boolean>(true);

  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    if (!chat) return;
    if (e.currentTarget.scrollTop === 0) {
      if (currChatPage === 1) return;
      setHitBtm(false)
      setCurrChatPage(currChatPage - 1);
      auth_net_get(
        `${memberEndpoint}?page=${currChatPage - 1}&pageSize=${ROOM_PAGE_SIZE}&filterOn=userId&filterBy=${auth?.user?.id}`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        const roomsArr: Room[] = data.data.map((e: Member) => e.room);
        chat?.setRooms(roomsArr);
      });
    }
    else if (Math.abs(Math.floor(e.currentTarget.scrollHeight - e.currentTarget.scrollTop) - e.currentTarget.clientHeight) < 5) {
      if (chat.activeRoomCount / ROOM_PAGE_SIZE < 1 || currChatPage >= chat.activeRoomCount / ROOM_PAGE_SIZE)
        return;
      setHitBtm(true);
      setCurrChatPage(currChatPage + 1);
      auth_net_get(
        `${memberEndpoint}?page=${currChatPage + 1}&pageSize=${ROOM_PAGE_SIZE}&filterOn=userId&filterBy=${auth?.user?.id}`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        const roomsArr: Room[] = data.data.map((e: Member) => e.room);
        chat?.setRooms(roomsArr);
        });
    }
  }

  useEffect(() => {
    if (hitBtm){
      const ref = document.getElementById("top-room");
      ref?.scrollIntoView();
    }
    else {
      const ref = document.getElementById("bottom-room");
      ref?.scrollIntoView({block: "end"});
    }
  }, [chat?.rooms]);

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
      <div className="h-96 overflow-scroll" onScroll={handleScroll}>
        <div className="my-5 py-5"></div>
        <div className="my-5 py-5"></div>
        <div className="my-5 py-5"></div>
        <div className="my-5 py-5" id="top-room"></div>
      {chat?.rooms?.map((room) => (
        <RoomCard 
        room={room}
        key = {room.id}
        />
      ))}
        <div className="my-5 py-5" id="bottom-room"></div>
      </div>

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
