import React, { FunctionComponent, useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router-dom";
import Alert, { AlertType } from "../../commonComponents/Alert";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChatWidget } from "../../context/chatWidgetContext";
import { auth_net_get } from "../../utils";
import ChatCard from "./ChatCard";
import GameInvBtn from "./GameInvBtn";

const memberEndpoint = `${API_ROOT}/members`;
const chatEndpoint = `${API_ROOT}/chat`;
const SCROLL_UP = 1;
const SCROLL_DOWN = -1;
const CHAT_PAGE_SIZE = 10;

const ActiveRoom: FunctionComponent = () => {
  const [user, setUser] = useState<User | null>(null); // pair object if room is a DM
  const [loading, setLoading] = useState<boolean>(false); // loading dm user info
  const [currChatPage, setCurrChatPage] = useState<number>(1); // current chat page
  const [scrollDirection, setScrollDirection] = useState(0); // current scroll direction
  const [currChatMsg, setCurrChatMsg] = useState<string>(""); // current chat message
  const [totalChats, setTotalChats] = useState<number>(-1); // total chat elemnts
  const [openAlert, setOpenAlert] = useState<AlertType>({
    type: "",
    isOpen: false,
  });
  const bottomRef = useRef<HTMLDivElement>(null);
  const topRef = useRef<HTMLDivElement>(null);
  const chatListRef = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();
  const auth = useAuth();
  const widget = useChatWidget();

  // initial render
  useEffect(() => {
    if (!widget?.currActiveRoom) {
      // reset all states to null
      setUser(null);
      return;
    }

    // scroll to bottom
    bottomRef.current?.scrollIntoView({ block: "end" });

    // if room is a dm, get opposing user
    const room = widget.currActiveRoom;
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
  }, [widget?.currActiveRoom]);

  //infinite scroll
  useEffect(() => {
    // get chat messages
    auth_net_get(
      `${chatEndpoint}?page=${currChatPage}&pageSize=${CHAT_PAGE_SIZE}&filterOn=roomId&filterBy=${widget?.currActiveRoom?.id}&sortBy=Descending&sortOn=createdAt`
    ).then((data) => {
      setTotalChats(data.total_elements);
      widget?.setActiveRoomMessages(data.data);
    });

    if (scrollDirection === SCROLL_DOWN) {
      // scroll to top
      topRef.current?.scrollIntoView({ block: "start" });
      return;
    } else {
      // scroll to bottom
      bottomRef.current?.scrollIntoView({ block: "end" });
      return;
    }
  }, [currChatPage]);

  // scroll handler
  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    if (e.currentTarget.scrollTop === 0) {
      if (totalChats / CHAT_PAGE_SIZE < 1 || currChatPage >= totalChats / CHAT_PAGE_SIZE)
        return;
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

  // chat message submit handler
  const handleSubmit = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const dto = {
      userId: auth?.user?.id,
      roomId: widget?.currActiveRoom?.id,
      message: currChatMsg,
    };

    auth?.chatSocket?.emit("message", JSON.stringify(dto));
    setCurrChatMsg("");
    setTotalChats(totalChats + 1);
  };

  return (
    <div className="grid grid-rows-6 h-full">
      {/* Header */}
      <div className="w-full shadow-md bg-slate-100 grid grid-cols-6 gap-4 row-span-1 h-full">
        <button
          className="col-span-1 flex justify-center items-center"
          onClick={(e) => {
            widget?.setCurrActiveRoom(null);
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
          {loading
            ? "loading..."
            : user
            ? user.username
            : widget?.currActiveRoom?.roomName}
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
          ‎
        </div>

        {/* Map backwards */}
        <div className="flex flex-col-reverse">
          {widget?.activeRoomMessages?.map((e, i) => (
            <ChatCard
              key={i}
              message={e}
              floatRight={e.userId === auth?.user?.id}
              system={!e.userId}
            />
          ))}

          {widget?.activeRoomMessages &&
          widget.activeRoomMessages.length < CHAT_PAGE_SIZE
            ? [...Array(CHAT_PAGE_SIZE - widget.activeRoomMessages.length)].map(
                (e, i) => (
                  <div key={i} className="py-5">
                    {" "}
                  </div>
                )
              )
            : null}
        </div>

        <div className="my-5" ref={bottomRef}>
          {" "}
          ‎
        </div>
      </div>

      {/* Chat input field */}
      <div className="row-span-1 h-full">
        {/* Alert feedback */}
        {openAlert.isOpen ? (
          <Alert
            alert={openAlert}
            setOpenAlert={setOpenAlert}
            message={"test"}
          />
        ) : null}
        <form className="grid grid-cols-6 h-full" onSubmit={handleSubmit}>
          <div className="h-full w-full col-span-5 flex items-center justify-center p-1">
            <input
              className="shadow w-full h-full px-3"
              value={currChatMsg}
              onChange={(e) => setCurrChatMsg(e.target.value)}
              type="text"
              placeholder="Message"
            />
          </div>
          <button className="w-full"> send </button>
        </form>
      </div>
    </div>
  );
};

export default ActiveRoom;
