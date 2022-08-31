import React, { FunctionComponent, useState } from "react";
import { NavLink } from "react-router-dom";
import {
  OUTGOING_LEAVE,
} from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { Member } from "../classes";

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

  const [currentMessage, setCurrentMessage] = useState("");

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

  return (
    <div className="h-96">
      <div>
        {chat?.activeRoom?.type === "DM" && memberUsers != null && memberUsers[1] != null && memberUsers[0] != null? (
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
      </div>
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
