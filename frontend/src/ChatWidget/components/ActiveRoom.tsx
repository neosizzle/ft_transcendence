import React, { FunctionComponent, useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { auth_net_get } from "../../utils";
import { Room } from "../types";
import GameInvBtn from "./GameInvBtn";

interface ActiveRoomProps {
  room: Room;
  setActiveRoom: React.Dispatch<React.SetStateAction<Room | null>>;
}

const memberEndpoint = `${API_ROOT}/members`;
const SCROLL_UP = 1;
const SCROLL_DOWN = -1;

const ActiveRoom: FunctionComponent<ActiveRoomProps> = ({
  room,
  setActiveRoom,
}) => {
  const [user, setUser] = useState<User | null>(null); // pair object if room is a DM
  const [loading, setLoading] = useState<boolean>(false); // loading dm user info
  const [currChatPage, setCurrChatPage] = useState<number>(1); // current chat page
  const [scrollDirection, setScrollDirection] = useState(0); // current scroll direction
  const bottomRef = useRef<HTMLDivElement>(null);
  const topRef = useRef<HTMLDivElement>(null);
  const chatListRef = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();
  const auth = useAuth();

  // initial render
  useEffect(() => {
    if (!room) {
      // reset all states to null
      setUser(null);
      return;
    }

    // scroll to bottom
    bottomRef.current?.scrollIntoView({ block: "end", behavior: "smooth" });

    // if room is a dm, get opposing user
    if (room.type === "DM") {
      setLoading(true);
      auth_net_get(
        `${memberEndpoint}?page=1&pageSize=2&filterOn=roomId&filterBy=${room.id}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        if (data.data[0].userId === auth?.user?.id) {
          setUser(data.data[1].user);
        } else {
          setUser(data.data[0].user);
        }
        setLoading(false);
      });
    }
  }, [room]);

  //infinite scroll
  useEffect(() => {
    if (scrollDirection === SCROLL_UP) {
      // scroll to bottom
      bottomRef.current?.scrollIntoView({ block: "end" });
      return;
    } else {
      // scroll to top
      topRef.current?.scrollIntoView({ block: "start" });
      return;
    }
  }, [currChatPage]);

  // scroll handler
  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {

    if (e.currentTarget.scrollTop === 0) {
      setCurrChatPage(currChatPage + 1);
      setScrollDirection(SCROLL_UP);
    } else if (
      Math.floor(e.currentTarget.scrollHeight - e.currentTarget.scrollTop) ===
      e.currentTarget.clientHeight
    ) {
      if (currChatPage == 1) return;
      setCurrChatPage(currChatPage - 1);
      setScrollDirection(SCROLL_DOWN);
    }
  };

  return (
    <div className="grid grid-rows-6 h-full">
      {/* Header */}
      <div className="w-full shadow-md bg-slate-100 grid grid-cols-6 gap-4 row-span-1 h-full">
        <button
          className="col-span-1 flex justify-center items-center"
          onClick={(e) => {
            setActiveRoom(null);
            e.stopPropagation();
          }}
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            className="h-6 w-6"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            strokeWidth={2}
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              d="M10 19l-7-7m0 0l7-7m-7 7h18"
            />
          </svg>
        </button>

        {/* Roomname / username */}
        <div className="col-span-4 flex items-center lg:text-xl">
          {" "}
          {loading ? "loading..." : user ? user.username : room.roomName}
        </div>

        {/* Invite to queue btn */}
        {user ? (
          <div className="col-span-1">
            <GameInvBtn user={user} />
          </div>
        ) : null}
      </div>

      {/* Chat content */}
      <div
        className="row-span-4 border-b-2 overflow-auto"
        onScroll={handleScroll}
        ref={chatListRef}
      >
        <div className="my-5" ref={topRef}>
          {" "}
          Blank div
        </div>
        {[...Array(10)].map((e, i) => (
          <div key={i} className="py-5">
            {" "}
            wee {i} curr chat apge {currChatPage}
          </div>
        ))}
        <div className="my-5" ref={bottomRef}>
          {" "}
          Blank div
        </div>
      </div>

      {/* Chat input field */}
      <div className="row-span-1">Input</div>
    </div>
  );
};

export default ActiveRoom;
