import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../constants";
import { useAuth } from "../context/authContext";
import { auth_net_get } from "../utils";
import ActiveRoom from "./components/ActiveRoom";
import RoomList from "./components/RoomList";
import { Member, Room } from "./types";

const membersEndpoint = `${API_ROOT}/members`;
const PAGE_SIZE = 4;

interface ChatWindowProps {
  open?: boolean;
}

const ChatWindow: FunctionComponent<ChatWindowProps> = ({ open }) => {
  const [activeRoom, setActiveRoom] = useState<Room | null>(null); // current active viewing room
  const [rooms, setRooms] = useState<Room[] | null>(null); // rooms joined
  const [currPage, setCurrPage] = useState<number>(1); // current page of rooms
  const [totalElements, setTotalElements] = useState<number>(-1); // total joined rooms
  const auth = useAuth();
  const navigate = useNavigate();

  // refreshes rooms evertime page changes
  useEffect(() => {
    // get rooms
    auth_net_get(
      `${membersEndpoint}?page=${currPage}&pageSize=${PAGE_SIZE}&filterOn=userId&filterBy=${auth?.user?.id}`
    )
      .then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        const rooms: Room[] = [];

        data.data.forEach((e: Member) => {
          rooms.push(e.room);
        });
        setRooms(rooms);
        setTotalElements(data.total_elements);
      })
      .catch((e) => console.error(e));
  }, [currPage]);

  return (
    <div
      id="chat-win"
      className={`${
        open ? "" : "hidden"
      } absolute bottom-full right-0 bg-white w-60 h-60 drop-shadow-xl rounded lg:rounded-none lg:h-96 lg:w-full mb-3 lg:mb-0`}
    >
      {activeRoom ? (
        // ActiveRoom
       <ActiveRoom room={activeRoom} setActiveRoom={setActiveRoom}/>
      ) : (
        // Roomlist
        <RoomList
          rooms={rooms}
          currPage={currPage}
          setCurrPage={setCurrPage}
          pageSize={PAGE_SIZE}
          totalElements={totalElements}
          setActiveRoom={setActiveRoom}
        />
      )}
    </div>
  );
};

export default ChatWindow;
