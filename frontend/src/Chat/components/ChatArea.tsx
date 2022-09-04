import React, { FunctionComponent, useEffect, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import {
  API_ROOT,
  OUTGOING_LEAVE,
} from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_get, auth_net_patch } from "../../utils";
import { Member, Message, roomPatchDto } from "../classes";

const chatEndpoint = `${API_ROOT}/chat`;
const roomEndpoint = `${API_ROOT}/rooms`

interface ChatAreaProps {
  members: Member[] | null;
  memberUsers: User[] | null;
  otherDmUser: User | null;
}

const ChatArea: FunctionComponent<ChatAreaProps> = ({
  members,
  memberUsers,
  otherDmUser,
}) => {
  const chat = useChat();
  const auth = useAuth();
  const navigate = useNavigate();

  const [currentMessage, setCurrentMessage] = useState("");
  const [currChatPage, setCurrChatPage] = useState<number>(1);
  const [hitBtm, setHitBtm] = useState<boolean>(false);

  const sendMessage = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (currentMessage !== "") {
      const dto = {
        userId: auth?.user?.id,
        roomId: chat?.activeRoom?.id,
        message: currentMessage,
      };
      auth?.chatSocket?.emit("message", JSON.stringify(dto));
      setCurrentMessage("");
    }
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setCurrentMessage(event.target.value);
  };

  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    if (!chat) return;
    if (e.currentTarget.scrollTop === 0) {
      setHitBtm(false)
      if (chat.activeRoomMessagesCount / 15 < 1 || currChatPage >= chat.activeRoomMessagesCount / 15)
        return;
      setCurrChatPage(currChatPage + 1);
      auth_net_get(
        `${chatEndpoint}?page=${currChatPage + 1}&pageSize=15&filterOn=roomId&filterBy=${chat?.activeRoom?.id}&sortBy=Descending&sortOn=createdAt`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const chats = data.data;
        const msgsArr: Message[] = [];
        chats.forEach((chat: Message) => {
          msgsArr.unshift(chat);
        });
        chat?.setActiveRoomMessages(msgsArr);
      });
    }
    else if (e.currentTarget.scrollHeight - e.currentTarget.scrollTop === e.currentTarget.clientHeight) {
      if (currChatPage === 1) return;
      setHitBtm(true)
      setCurrChatPage(currChatPage - 1);
      auth_net_get(
        `${chatEndpoint}?page=${currChatPage - 1}&pageSize=15&filterOn=roomId&filterBy=${chat?.activeRoom?.id}&sortBy=Descending&sortOn=createdAt`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const chats = data.data;
        const msgsArr: Message[] = [];
        chats.forEach((chat: Message) => {
          msgsArr.unshift(chat);
        });
        chat?.setActiveRoomMessages(msgsArr);
      });
    }
  }

  useEffect(() => {
    if (!chat?.activeRoomMessages) return;
    if (hitBtm){
      const test = document.getElementById("top");
      test?.scrollIntoView();
    }
    else {
      const test = document.getElementById("bottom");
      test?.scrollIntoView({block: "end"});
    }
  }, [chat?.activeRoomMessages]);

  //reset hitbtm once room changes
  useEffect(()=> {
    setHitBtm(false)
    setCurrChatPage(1);
  }, [chat?.activeRoom])

  return (
    <div className="">
      <div className="flex flex-row">
        {chat?.activeRoom?.type === "DM" && memberUsers != null && memberUsers[1] != null && memberUsers[0] != null ? (
          auth?.user?.id === memberUsers[0].id ? (
            <p className="text-3xl">{memberUsers[1].username} </p>
          ) : (
            <p className="text-3xl ">{memberUsers[0].username} </p>
          )
        ) : (
          <p className="text-3xl ">
            {chat?.activeRoom?.roomName} - {chat?.activeRoom?.id}
          </p>
        )}
      </div>
      <div className="flex flex-row justify-around">
        {/* Profile btn */}
        {chat?.activeRoom?.type === "DM" ? (
          <NavLink
            to={`/users/profile/${otherDmUser?.id}`}
            className="block rounded px-4 py-2 bg-gray-400"
          >
            View Profile
          </NavLink>
        ) : null}

        {/* Leave room Btn */}
        {chat?.activeRoom?.type === "GC" ? (
          <button
            onClick={() => {
              // get member id
              const memberId = members?.find(
                (member) => member.userId === auth?.user?.id
              )?.id;
              if (!memberId) return;

              // emit leave room dto
              auth?.chatSocket?.emit(OUTGOING_LEAVE, memberId.toString());
            }}
            className="block rounded px-4 py-2 bg-gray-400"
          >
            Leave
          </button>
        ) : null}

       {/* Edit room btn */}
       {chat?.activeRoom?.ownerId === auth?.user?.id ? (
          <button
            onClick={() => {
              const newRoomName = prompt("enter new room name (leave blank if change not wanted)")
              const newPassword = prompt("enter new room password (leave blank if want to remove password)")

              const payload : roomPatchDto = {};
              if (newRoomName && newRoomName.length > 0) 
                payload.roomName = newRoomName;
              if (!newPassword)
                payload.isProtected = false;
              else if (newPassword.length < 7)
              {
                alert("Password must be more than 7 characters in length");
                return ;
              }
              else
                payload.password = newPassword;
              auth_net_patch(`${roomEndpoint}/${chat?.activeRoom?.id}`, payload)
              .then((data) => {
                 // token expired
                  if (data.error && data.error == "Forbidden") return navigate("/logout");

                  if (data.error)
                  {
                    chat?.setAlertMessage(data.error)
                    chat?.setOpenAlert({type : "error", isOpen : true})
                    return;
                  }
                  chat?.setAlertMessage("Room details changes successfully");
                  chat?.setOpenAlert({type : "success", isOpen : true})
              })
            }}
            className="block rounded px-4 py-2 bg-gray-400"
          >
            Edit room
          </button>
        ) : null}


      </div>
      <div className="h-96 overflow-scroll" onScroll={handleScroll}>
        <div className="my-5 py-5" id="top"></div>
        {chat?.activeRoomMessages?.map((message) => (
          <div key={message.id}>
            {message.userId === auth?.user?.id ? (
              <div>
                <div className="text-lg text-right">{message.message}</div>
                <div className="text-xs text-grey text-right">
                  {new Date(message.createdAt).toLocaleString("en-gb", {
                    hour12: true,
                  })}
                </div>
              </div>
            ) : (
              <div>
                <div className="text-lg">{message.message}</div>
                <div className="text-xs text-grey">
                  {new Date(message.createdAt).toLocaleString("en-gb", {
                    hour12: true,
                  })}
                </div>
              </div>
            )}
          </div>
        ))}
        <div className="my-5 py-5" id="bottom"></div>
      </div>
      <form onSubmit={sendMessage} className="Chat">
        <input
          type="text"
          className="w-fit mt-10 border-2"
          placeholder="Say something..."
          id="message"
          onChange={handleChange}
          value={currentMessage}
        />
      </form>
      {/* </div> */}
    </div>
  );
};

export default ChatArea;
