import React, { FunctionComponent, useState } from "react";
import { Room } from "../../Chat/classes";
import { OUTGOING_GET_QUEUE, OUTGOING_INV} from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChatWidget } from "../../context/chatWidgetContext";
import { QueueInfo } from "../../Game/Canvas";

interface GameInvBtnProps {
  user: User;
  room: Room;
}

const GameInvBtn: FunctionComponent<GameInvBtnProps> = ({ user, room }) => {
  const [hover, setHover] = useState<boolean>(false);
  const auth = useAuth();
  const widget = useChatWidget();

  return (
    <div className="h-full relative">
      {hover ? (
        <div className="absolute -top-2 -left-8 w-24 px-2 py-1 text-xs rounded bg-gray-400">
          Invite to queue
        </div>
      ) : null}
      <button
        className="w-full h-full flex justify-center items-center hover:bg-slate-200 active:bg-slate-400"
        onMouseEnter={() => setHover(true)}
        onMouseLeave={() => setHover(false)}
        onClick={(e) => {
          // If user is not in queue, reject action
          auth?.gameSocket?.emit(OUTGOING_GET_QUEUE, (queue : QueueInfo) => {
            if (queue.position[0] === -1 && queue.position[1] === -1 )
            {
              widget?.setAlertMessage("User not in queue")
              widget?.setOpenAlert({type : "error", isOpen : true})
              return ;
            }

            //determines if invited to queue 0 or queue 1
            let queuePosition : number;
            if (queue.position[0] > -1 ) queuePosition = 1;
            else queuePosition = 0
            
            auth?.chatWidgetSocket?.emit(
              OUTGOING_INV,
              JSON.stringify({ userId: user.id, roomId: room.id, queuePosition })
            );
          })
          e.stopPropagation();
        }}
      >
        <svg
          xmlns="http://www.w3.org/2000/svg"
          className="h-1/2 w-1/2"
          viewBox="0 0 20 20"
          fill="currentColor"
        >
          <path d="M8 9a3 3 0 100-6 3 3 0 000 6zM8 11a6 6 0 016 6H2a6 6 0 016-6zM16 7a1 1 0 10-2 0v1h-1a1 1 0 100 2h1v1a1 1 0 102 0v-1h1a1 1 0 100-2h-1V7z" />
        </svg>
      </button>
    </div>
  );
};

export default GameInvBtn;
