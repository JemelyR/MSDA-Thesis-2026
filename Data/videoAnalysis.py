# Jocher, G., Qiu, J., & Chaurasia, A. (2023). Ultralytics YOLO (Version 8.0.0)
# [Software]. Ultralytics. https://ultralytics.com

import cv2
import json
import csv
import argparse
import os
import glob
import numpy as np
from pathlib import Path
from dataclasses import dataclass, asdict
from ultralytics import YOLO

@dataclass
class FrameMetrics:
    frame_idx: int
    timestamp_sec: float
    detected: bool
    bbox_x: float
    bbox_y: float
    bbox_w: float
    bbox_h: float
    bbox_area_ratio: float
    center_x: float
    center_y: float
    centrality: float
    confidence: float

def get_condition(name: str) -> str:
    prefix = name.upper()[0] if name else ""
    if prefix == "T":
        return "Test"
    elif prefix == "N":
        return "Control"
    return "Unknown"

def centrality_score(cx: float, cy: float) -> float:
    dist = np.sqrt((cx - 0.5) ** 2 + (cy - 0.5) ** 2)
    return float(np.clip(1.0 - dist / 0.707, 0, 1))

def analyze_frame(frame, frame_idx, fps, model, conf_threshold=0.65):
    h, w = frame.shape[:2]
    ts = frame_idx / fps

    results = model(frame, verbose=False, conf=conf_threshold)[0]

    if len(results.boxes) == 0:
        return FrameMetrics(frame_idx, ts, False, 0, 0, 0, 0, 0, 0.5, 0.5, 0, 0)

    boxes    = results.boxes
    best_idx = int(boxes.conf.argmax())
    box      = boxes.xyxy[best_idx].cpu().numpy()
    conf     = float(boxes.conf[best_idx].cpu())

    x1, y1, x2, y2 = box
    bw = x2 - x1
    bh = y2 - y1
    nx   = x1 / w
    ny   = y1 / h
    nw   = bw / w
    nh   = bh / h
    ar   = nw * nh
    cx   = nx + nw / 2
    cy   = ny + nh / 2
    cent = centrality_score(cx, cy)

    return FrameMetrics(frame_idx, ts, True, nx, ny, nw, nh, ar, cx, cy, cent, conf)

def process_video(video_path, output_dir, model, sample_every=1, save_individual=True):
    cap   = cv2.VideoCapture(video_path)
    fps   = cap.get(cv2.CAP_PROP_FPS) or 30.0
    total = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    fw    = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    fh    = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    name  = Path(video_path).stem
    cond  = get_condition(name)

    print(f"Processing: {name}")
    os.makedirs(output_dir, exist_ok=True)

    all_metrics = []
    frame_idx   = 0

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        if frame_idx % sample_every == 0:
            m = analyze_frame(frame, frame_idx, fps, model)
            all_metrics.append(m)

        frame_idx += 1

    cap.release()

    csv_path = os.path.join(output_dir, f"{name}_frame_data.csv")
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(asdict(all_metrics[0]).keys()))
        writer.writeheader()
        for m in all_metrics:
            writer.writerow(asdict(m))

    detected = [m for m in all_metrics if m.detected]
    eff_fps  = fps / sample_every
    dur_sec  = len(all_metrics) / eff_fps
    on_sec   = len(detected) / eff_fps

    avg_cent = float(np.mean([m.centrality for m in detected])) if detected else 0
    avg_conf = float(np.mean([m.confidence for m in detected])) if detected else 0

    max_run = cur = 0
    for m in all_metrics:
        cur     = cur + 1 if m.detected else 0
        max_run = max(max_run, cur)

    summary = {
        "participant":               name,
        "condition":                 cond,
        "on_screen_time_sec":        round(on_sec, 2),
        "total_duration_sec":        round(dur_sec, 2),
        "longest_continuous_sec":    round(max_run / eff_fps, 2),
        "avg_centrality_score":      round(avg_cent, 3),
        "avg_detection_confidence":  round(avg_conf, 3),
        "frames_character_detected": len(detected),
        "total_frames_analysed":     len(all_metrics),
    }

    if save_individual:
        json_path = os.path.join(output_dir, f"{name}_summary.json")
        with open(json_path, "w") as f:
            json.dump(summary, f, indent=2)

    print(f"Done: {name}")
    return summary

def batch_process(folder, output_dir, model, sample_every=1):
    patterns = ["*.mov", "*.MOV"]
    videos   = []
    for p in patterns:
        videos.extend(glob.glob(os.path.join(folder, p)))
    videos = sorted(set(videos))

    if not videos:
        print(f"No videos in {folder}")
        return

    print(f"{len(videos)} videos.\n")
    all_summaries = []

    for i, v in enumerate(videos, 1):
        print(f"[{i}/{len(videos)}] ", end="", flush=True)
        try:
            s = process_video(v, output_dir, model, sample_every, save_individual=False)
            all_summaries.append(s)
        except Exception as e:
            print(f"ERROR: {Path(v).stem} — {e}")

    if all_summaries:
        batch_csv = os.path.join(output_dir, "video_batch_summary.csv")
        with open(batch_csv, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=list(all_summaries[0].keys()))
            writer.writeheader()
            writer.writerows(all_summaries)
        print(f"\nDone.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Character visibility analysis using YOLO.")
    parser.add_argument("--video",  type=str, help="Path to a single video file")
    parser.add_argument("--folder", type=str, help="Folder of videos (batch mode)")
    parser.add_argument("--model",  type=str, required=True,
                        help="Path to trained best.pt")
    parser.add_argument("--output", type=str, default="output",
                        help="Output directory (default: ./output)")
    parser.add_argument("--sample", type=int, default=1,
                        help="Analyse every Nth frame (default 1).")
    args = parser.parse_args()

    print(f"Loading model: {args.model}")
    model = YOLO(args.model)

    if args.folder:
        batch_process(args.folder, args.output, model, args.sample)
    elif args.video:
        process_video(args.video, args.output, model, args.sample)
    else:
        print("Use --video or --folder. Use --help for options.")

        