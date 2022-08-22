import React, { FunctionComponent, useEffect, useState } from "react";
import { NavigateFunction, useNavigate } from "react-router-dom";
import { Member, Message, Room } from "../Chat/classes";
import Alert from "../commonComponents/Alert";
import { API_ROOT } from "../constants";
import { useAuth } from "../context/authContext";
import { useChatWidget } from "../context/chatWidgetContext";
import { auth_net_get } from "../utils";
import ActiveRoom from "./components/ActiveRoom";
import RoomList from "./components/RoomList";

const membersEndpoint = `${API_ROOT}/members`;
const chatEndpoint = `${API_ROOT}/chat`;
const PAGE_SIZE = 4;

interface ChatWindowProps {
  open?: boolean;
}

const generateLastMessages = async (
  rooms: Room[] | null | undefined,
  navigate: NavigateFunction
) => {
  const lastMessages: Message[] = [];

  if (!rooms) return lastMessages;
  for (let index = 0; index < rooms.length; index++) {
    const room = rooms[index];
    const message = await auth_net_get(
      `${chatEndpoint}?page=1&pageSize=1&sortBy=Descending&sortOn=createdAt&filterOn=roomId&filterBy=${room.id}`
    );
    // check user unauth
    if (message.error && message.error == "Forbidden")
      return navigate("/logout");

    lastMessages.push(message.data[0] || null);
  }
  return lastMessages;
};

const ChatWindow: FunctionComponent<ChatWindowProps> = ({ open }) => {
  const [currPage, setCurrPage] = useState<number>(1); // current page of rooms
  const [totalElements, setTotalElements] = useState<number>(-1); // total joined rooms
  const auth = useAuth();
  const navigate = useNavigate();
  const widget = useChatWidget();

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
        widget?.setRooms(rooms);
        setTotalElements(data.total_elements);
      })
      .catch((e) => console.error(e));
  }, [currPage]);

  // refreshes last messages everytime room changes
  useEffect(() => {
    generateLastMessages(widget?.rooms, navigate).then((data) =>
      widget?.setLastMessages(data as Message[])
    );
  }, [widget?.rooms]);

  return (
    <div
      id="chat-win"
      className={`${
        open ? "" : "hidden"
      } absolute bottom-full right-0 bg-white w-60 h-60 drop-shadow-xl rounded lg:rounded-none lg:h-96 lg:w-full mb-3 lg:mb-0`}
    >
      {/* Alert Notification */}
      {widget?.openAlert.isOpen ? (
        <Alert
          alert={widget.openAlert}
          setOpenAlert={widget.setOpenAlert}
          message={widget.alertMessage}
        />
      ) : null}
      
      {widget?.currActiveRoom ? (
        // ActiveRoom
        <ActiveRoom />
      ) : (
        // Roomlist
        <RoomList
          rooms={widget?.rooms}
          currPage={currPage}
          setCurrPage={setCurrPage}
          pageSize={PAGE_SIZE}
          totalElements={totalElements}
        />
      )}

    </div>
  );
};

export default ChatWindow;
